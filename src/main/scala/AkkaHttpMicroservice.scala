import akka.actor.ActorSystem
import akka.event.{Logging, LoggingAdapter}
import akka.http.scaladsl.Http
import akka.http.scaladsl.client.RequestBuilding
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.{HttpRequest, HttpResponse, Multipart}
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.{ActorMaterializer, Materializer}
import akka.stream.scaladsl.{Flow, Sink, Source}
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import java.io.IOException

import akka.util.ByteString

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.math._
import spray.json.DefaultJsonProtocol
import tsimporter.Main

case class IpInfo(query: String, country: Option[String], city: Option[String], lat: Option[Double], lon: Option[Double])

case class IpPairSummaryRequest(ip1: String, ip2: String)

case class IpPairSummary(distance: Option[Double], ip1Info: IpInfo, ip2Info: IpInfo)

object IpPairSummary {
  def apply(ip1Info: IpInfo, ip2Info: IpInfo): IpPairSummary = IpPairSummary(calculateDistance(ip1Info, ip2Info), ip1Info, ip2Info)

  private def calculateDistance(ip1Info: IpInfo, ip2Info: IpInfo): Option[Double] = {
    (ip1Info.lat, ip1Info.lon, ip2Info.lat, ip2Info.lon) match {
      case (Some(lat1), Some(lon1), Some(lat2), Some(lon2)) =>
        // see http://www.movable-type.co.uk/scripts/latlong.html
        val φ1 = toRadians(lat1)
        val φ2 = toRadians(lat2)
        val Δφ = toRadians(lat2 - lat1)
        val Δλ = toRadians(lon2 - lon1)
        val a = pow(sin(Δφ / 2), 2) + cos(φ1) * cos(φ2) * pow(sin(Δλ / 2), 2)
        val c = 2 * atan2(sqrt(a), sqrt(1 - a))
        Option(EarthRadius * c)
      case _ => None
    }
  }

  private val EarthRadius = 6371.0
}

trait Protocols extends DefaultJsonProtocol {
  implicit val ipInfoFormat = jsonFormat5(IpInfo.apply)
  implicit val ipPairSummaryRequestFormat = jsonFormat2(IpPairSummaryRequest.apply)
  implicit val ipPairSummaryFormat = jsonFormat3(IpPairSummary.apply)
}

trait Service extends Protocols {
  implicit val system: ActorSystem
  implicit def executor: ExecutionContextExecutor
  implicit val materializer: Materializer

  def config: Config
  val logger: LoggingAdapter

  lazy val ipApiConnectionFlow: Flow[HttpRequest, HttpResponse, Any] =
    Http().outgoingConnection(config.getString("services.ip-api.host"), config.getInt("services.ip-api.port"))

  def ipApiRequest(request: HttpRequest): Future[HttpResponse] = Source.single(request).via(ipApiConnectionFlow).runWith(Sink.head)

  def fetchIpInfo(ip: String): Future[Either[String, IpInfo]] = {
    ipApiRequest(RequestBuilding.Get(s"/json/$ip")).flatMap { response =>
      response.status match {
        case OK => Unmarshal(response.entity).to[IpInfo].map(Right(_))
        case BadRequest => Future.successful(Left(s"$ip: incorrect IP format"))
        case _ => Unmarshal(response.entity).to[String].flatMap { entity =>
          val error = s"FreeGeoIP request failed with status code ${response.status} and entity $entity"
          logger.error(error)
          Future.failed(new IOException(error))
        }
      }
    }
  }

  val routes = {

    logRequestResult("akka-http-microservice") {
      path("upload") {
        entity(as[Multipart.FormData]) { (formdata: Multipart.FormData) ⇒
          complete {

            formdata.parts.mapAsync(1) { p ⇒
              println(s"Got part. name: ${p.name} filename: ${p.filename}")

              println("Counting size...")
              @volatile var lastReport = System.currentTimeMillis()
              @volatile var lastSize = 0L
              def receiveChunk(counter: (Long, Long), chunk: ByteString): (Long, Long) = {
                val (oldSize, oldChunks) = counter
                val newSize = oldSize + chunk.size
                val newChunks = oldChunks + 1

                val now = System.currentTimeMillis()
                if (now > lastReport + 1000) {
                  val lastedTotal = now - lastReport
                  val bytesSinceLast = newSize - lastSize
                  val speedMBPS = bytesSinceLast.toDouble / 1000000 /* bytes per MB */ / lastedTotal * 1000 /* millis per second */

                  println(f"Already got $newChunks%7d chunks with total size $newSize%11d bytes avg chunksize ${newSize / newChunks}%7d bytes/chunk speed: $speedMBPS%6.2f MB/s")

                  lastReport = now
                  lastSize = newSize
                }
                (newSize, newChunks)
              }

//              Main.a(p.entity.dataBytes)
              p.entity.dataBytes.runFold((0L, 0L))(receiveChunk).map {
                case (size, numChunks) ⇒
                  println(s"Size is $size")
                  (p.name, p.filename, size)
              }
            }.runFold(Seq.empty[(String, Option[String], Long)])(_ :+ _).map(_.mkString(", "))
          }
        }
      } ~
      pathPrefix("ip") {
        (get & path(Segment)) { ip =>
          complete {
            fetchIpInfo(ip).map[ToResponseMarshallable] {
              case Right(ipInfo) => ipInfo
              case Left(errorMessage) => BadRequest -> errorMessage
            }
          }
        } ~
        (post & entity(as[IpPairSummaryRequest])) { ipPairSummaryRequest =>
          complete {
            val ip1InfoFuture = fetchIpInfo(ipPairSummaryRequest.ip1)
            val ip2InfoFuture = fetchIpInfo(ipPairSummaryRequest.ip2)
            ip1InfoFuture.zip(ip2InfoFuture).map[ToResponseMarshallable] {
              case (Right(info1), Right(info2)) => IpPairSummary(info1, info2)
              case (Left(errorMessage), _) => BadRequest -> errorMessage
              case (_, Left(errorMessage)) => BadRequest -> errorMessage
            }
          }
        }
      }
    }
  }
}

object AkkaHttpMicroservice extends App with Service {
  override implicit val system = ActorSystem()
  override implicit val executor = system.dispatcher
  override implicit val materializer = ActorMaterializer()

  override val config = ConfigFactory.load()
  override val logger = Logging(system, getClass)

  Http().bindAndHandle(routes, config.getString("http.interface"), config.getInt("http.port"))
}
