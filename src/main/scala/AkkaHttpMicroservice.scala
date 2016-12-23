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
import akka.stream.scaladsl.{Flow, Sink, Source, StreamConverters}
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import java.io._
import java.text.{DateFormat, SimpleDateFormat}
import java.util.Date
import java.util.concurrent.TimeUnit

import akka.util.ByteString

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.math._
import spray.json.DefaultJsonProtocol
import importer.{FramerParser, ParseResult}

import scala.collection.immutable.PagedSeq
import scala.concurrent.duration.FiniteDuration
import scala.util.parsing.input.PagedSeqReader
import akka.http.scaladsl.server.directives.ExecutionDirectives._
import ch.megard.akka.http.cors.CorsDirectives._
import ch.megard.akka.http.cors.CorsSettings
import com.qiniu.common.{QiniuException, Zone}
import com.qiniu.storage.{Configuration, UploadManager}
import com.qiniu.util.Auth
import utl.Unzip

trait Protocols extends DefaultJsonProtocol {
  implicit val parserResultFormat = jsonFormat2(ParseResult.apply)
}

trait Service extends Protocols {
  implicit val system: ActorSystem
  implicit def executor: ExecutionContextExecutor
  implicit val materializer: Materializer
  implicit val uploadManager: UploadManager;
  implicit val token: String;
  implicit val CdnUrl: String;

  def config: Config

  val logger: LoggingAdapter

  def generatorHtml(
                     parserResult: ParseResult): ParseResult = {
    val a: String =
      "<html>\n" +
        "<head>\n" +
        "<style type=\"text/css\" media=\"screen\">\n" +
        parserResult.css +
        "</style>" + "\n" +
        "</head>\n" +
        "<body>" + "\n" +
        parserResult.html +
        "\n" +
        "\t</body>\n" +
        "</html>"
    ParseResult(a, parserResult.css)
  }
  val routes = {
    logRequestResult("dumframer") {
      path("uploadzip") {
        entity(as[Multipart.FormData]) { (formdata: Multipart.FormData) ⇒
          complete {
            formdata.parts.mapAsync(1) { p ⇒
              val inputStream = p.entity.dataBytes.runWith(
                StreamConverters.asInputStream(FiniteDuration(3, TimeUnit.SECONDS))
              )
              val date = new Date
              val df = new SimpleDateFormat("MM_dd_yyyy_HH_mm_ss");
              val b = new File("/tmp/my-zip" + "/" + df.format(date))
              Unzip.unzip(inputStream, b.toPath)
              val reader = PagedSeq.fromReader(new InputStreamReader(new FileInputStream(b.getAbsoluteFile + "/" + "app.coffee")))
              val f = new File(b.getAbsoluteFile + "/" + "images")
              // returns pathnames for files and directory
              val paths = f.listFiles();
              // for each pathname in pathname array
              for (path <- paths) {
                try {
                  uploadManager.put(path.getAbsolutePath, df.format(date) + path.getName, token)
                } catch {
                  case e: QiniuException =>
                    e.printStackTrace()
                }
              }
              val a = new PagedSeqReader(reader);
              Future.successful(FramerParser.parse(a, Some(CdnUrl + df.format(date))))
            }.runFold(ParseResult("", ""))((a, b) => ParseResult(a.html + b.html, a.css + b.css)).map(generatorHtml)
          }
        }
      } ~
        path("upload") {
          entity(as[Multipart.FormData]) { (formdata: Multipart.FormData) ⇒
            complete {
              formdata.parts.mapAsync(1) { p ⇒
                val inputStream = p.entity.dataBytes.runWith(
                  StreamConverters.asInputStream(FiniteDuration(3, TimeUnit.SECONDS))
                )
                val reader = PagedSeq.fromReader(new InputStreamReader(inputStream))
                val a = new PagedSeqReader(reader);
                Future.successful(FramerParser.parse(a, Option.empty[String]))
              }.runFold(ParseResult("", ""))((a, b) => ParseResult(a.html + b.html, a.css + b.css)).map(generatorHtml)
            }
          }
        }
    }
  }
}

object AkkaHttpMicroservice extends App with Service {
  val AccessKey = "31IUl_ItncsjETFG8OIl902ebArYoaafs4q6g56u"
  val SecretKey = "3iW8-rI-FtGUP_di_fjaDOVi_Msj7f1VZE_siL_O"
  val BucketName = "tingshuo"
  val auth = Auth.create(AccessKey, SecretKey);
  val z = Zone.zone0();
  val c = new Configuration(z);
  override implicit val CdnUrl = "http://7xk03v.com1.z0.glb.clouddn.com/"
  override implicit val token = auth.uploadToken(BucketName)
  override implicit val uploadManager = new UploadManager(c)
  override implicit val system = ActorSystem()
  override implicit val executor = system.dispatcher
  override implicit val materializer = ActorMaterializer()
  override val config = ConfigFactory.load()
  override val logger = Logging(system, getClass)
  val settings = CorsSettings.defaultSettings.copy(allowGenericHttpRequests = true)
  Http().bindAndHandle(cors(settings)(routes), config.getString("http.interface"), config.getInt("http.port"))
}
