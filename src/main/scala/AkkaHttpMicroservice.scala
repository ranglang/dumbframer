import java.io._
import java.text.SimpleDateFormat
import java.util.Date
import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.event.{Logging, LoggingAdapter}
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.model.Multipart
import akka.http.scaladsl.server.Directives._
import akka.stream.scaladsl.StreamConverters
import akka.stream.{ActorMaterializer, Materializer}
import akka.util.Timeout
import ch.megard.akka.http.cors.CorsDirectives._
import ch.megard.akka.http.cors.CorsSettings
import com.qiniu.common.{QiniuException, Zone}
import com.qiniu.storage.{Configuration, UploadManager}
import com.qiniu.util.Auth
import com.typesafe.config.{Config, ConfigFactory}
import importer.{FramerParser, ParseResult}
import org.apache.commons.io.FileUtils
import spray.json.{DefaultJsonProtocol, _}
import utl.{FramerConfig, Unzip}

import scala.collection.immutable.PagedSeq
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.parsing.input.PagedSeqReader

trait Protocols extends DefaultJsonProtocol {
  implicit val parserResultFormat = jsonFormat2(ParseResult.apply)
  implicit val parserFramerConfig = jsonFormat3(FramerConfig.apply)
}

trait Service extends Protocols {
  implicit val system: ActorSystem
  implicit val timeout = Timeout(10.minute)

  implicit def executor: ExecutionContextExecutor

  implicit val materializer: Materializer
  implicit val uploadManager: UploadManager;
  implicit val token: String;
  implicit val CdnUrl: String;

  def config: Config

  val logger: LoggingAdapter

  def generatorHtml(parserResult: ParseResult): ParseResult = {
    val a: String =
      "<html>\n" +
        "<head>\n" +
        "<style type=\"text/css\" src=\"https://cdnjs.cloudflare.com/ajax/libs/normalize/5.0.0/normalize.min.css\"></style>"+
        "<script src=\"http://g.tbcdn.cn/mtb/lib-flexible/0.3.2/??flexible_css.js,flexible.js\"></script>" +
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

  def readDeviceType(framerConfigFile: File): Future[FramerConfig] = {
    val conFig = Future {
      val lines: String = FileUtils.readFileToString(framerConfigFile, "UTF-8");
      val config: FramerConfig = JsonParser(lines).convertTo[FramerConfig]
      config
    }
    conFig.recover {
      case e: Exception => FramerConfig(deviceType = "apple-iphone-5s-gold", projectId = "")
    }
  }

  def uploadImage(f: File, date: Date): Future[Unit] = {
    val df = new SimpleDateFormat("MM_dd_yyyy_HH_mm_ss")
    val paths = f.listFiles();
    // for each pathname in pathname array
    Future {
      for (path <- paths) {
        try {
          uploadManager.put(path.getAbsolutePath, df.format(date) + path.getName, token)
        } catch {
          case e: QiniuException =>
            e.printStackTrace()
        }
      }
    }
  }

  val routes = {
    logRequestResult("dumframer") {
      path("ip") {
        get {
          complete {
            Future.successful("ip")
          }
        }
      } ~
        path("uploadVnodezip") {
          entity(as[Multipart.FormData]) { (formdata: Multipart.FormData) ⇒
            complete {
              formdata.parts.mapAsync(1) { p ⇒ {
                p.entity.toStrict(1.minute).flatMap(str => {
                  val inputStream = str.dataBytes.runWith(
                    StreamConverters.asInputStream(FiniteDuration(100, TimeUnit.SECONDS))
                  )
                  val date = new Date
                  val df = new SimpleDateFormat("MM_dd_yyyy_HH_mm_ss");
                  val b = new File("/tmp/my-zip" + "/" + df.format(date))
                  Unzip.unzip(inputStream, b.toPath)
                  val reader = PagedSeq.fromReader(new InputStreamReader(new FileInputStream(b.getAbsoluteFile + "/" + "app.coffee")))
                  val f = new File(b.getAbsoluteFile + "/" + "images")
                  for {
                    framerConfig <- readDeviceType(f)
                    uploadImage <- uploadImage(f, date)
                  } yield {
                    val nFramerConfig = framerConfig.copy(projectId = CdnUrl + df.format(date))
                    val a = new PagedSeqReader(reader);
                    FramerParser.parseVnode(a, nFramerConfig)
                  }
                }
                )
              }
              }.runFold(ParseResult("", ""))((a, b) => ParseResult(a.html + b.html, a.css + b.css)) //.map(generatorHtml)
            }
          }
        } ~
        path("uploadzip") {
          entity(as[Multipart.FormData]) { (formdata: Multipart.FormData) ⇒
            println('uploadzip)
            complete {
              formdata.parts.mapAsync(1) { p ⇒ {
                p.entity.toStrict(1.minute).flatMap(str => {
                  val inputStream = str.dataBytes.runWith(
                    StreamConverters.asInputStream(FiniteDuration(100, TimeUnit.SECONDS))
                  )
                  val date = new Date
                  val df = new SimpleDateFormat("MM_dd_yyyy_HH_mm_ss");
                  val b = new File("/tmp/my-zip" + "/" + df.format(date))
                  Unzip.unzip(inputStream, b.toPath)
                  val reader = PagedSeq.fromReader(new InputStreamReader(new FileInputStream(b.getAbsoluteFile + "/" + "app.coffee")))
                  val f = new File(b.getAbsoluteFile + "/" + "images")
                  for {
                    framerConfig <- readDeviceType(f)
                    uploadImage <- uploadImage(f, date)
                  } yield {
                    val nFramerConfig = framerConfig.copy(projectId = CdnUrl + df.format(date))
                    val a = new PagedSeqReader(reader);
                    FramerParser.parse(a, nFramerConfig)
                  }
                }
                )
              }
            }.runFold(ParseResult("", ""))((a, b) => ParseResult(a.html + b.html, a.css + b.css)).map(generatorHtml)
          }
        }
    } ~
        path("uploadVnode") {
          entity(as[Multipart.FormData]) { (formdata: Multipart.FormData) ⇒
            complete {
              formdata.parts.mapAsync(1) { p ⇒
                p.entity.toStrict(5.minute).map(str => {
                  val inputStream = str.dataBytes.runWith(
                    StreamConverters.asInputStream(FiniteDuration(3, TimeUnit.SECONDS)))
                  val reader = PagedSeq.fromReader(new InputStreamReader(inputStream))
                  val a = new PagedSeqReader(reader);
                  FramerParser.parseVnode(a, FramerConfig("apple-iphone-5s-gold", ""))
                }
                )
              }.runFold(ParseResult("", ""))((a, b) => ParseResult(a.html + b.html, a.css + b.css)) //.map(generatorHtml)
            }
          }
        }~
      path("upload") {
        entity(as[Multipart.FormData]) { (formdata: Multipart.FormData) ⇒
          complete {
            formdata.parts.mapAsync(1) { p ⇒
              p.entity.toStrict(5.minute).map(str => {
                val inputStream = str.dataBytes.runWith(
                  StreamConverters.asInputStream(FiniteDuration(3, TimeUnit.SECONDS)))
                val reader = PagedSeq.fromReader(new InputStreamReader(inputStream))
                val a = new PagedSeqReader(reader);
                FramerParser.parse(a, FramerConfig("apple-iphone-5s-gold", ""))
              }
              )
            }.runFold(ParseResult("", ""))((a, b) => ParseResult(a.html + b.html, a.css + b.css)).map(generatorHtml)
          }
        }
      }
  }
}

}

object AkkaHttpMicroservice extends App with Service {
  println("AkkaHttpMicroservice ")
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
