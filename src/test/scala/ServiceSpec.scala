import java.io.File

import akka.event.NoLogging
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.model._
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.stream.scaladsl.Source
import com.qiniu.common.Zone
import com.qiniu.storage.{Configuration, UploadManager}
import com.qiniu.util.Auth
import org.scalatest._

import scala.concurrent.{Await, Future}

class ServiceSpec extends FlatSpec with Matchers with ScalatestRouteTest with Service {
  override def testConfigSource = "akka.loglevel = WARNING"

  override def config = testConfig

  override val logger = NoLogging

  val AccessKey = "31IUl_ItncsjETFG8OIl902ebArYoaafs4q6g56u"
  val SecretKey = "3iW8-rI-FtGUP_di_fjaDOVi_Msj7f1VZE_siL_O"
  val BucketName = "tingshuo"
  val auth = Auth.create(AccessKey, SecretKey);
  val z = Zone.zone0();
  val c = new Configuration(z);

  override implicit val CdnUrl = "http://7xk03v.com1.z0.glb.clouddn.com/"
  override implicit val token = auth.uploadToken(BucketName)
  override implicit val uploadManager = new UploadManager(c)
  import akka.actor.ActorSystem

  import scala.concurrent.duration._
//  dilated
  implicit def default(implicit system: ActorSystem) = RouteTestTimeout(100.second)
  akka.http.scaladsl.testkit.RouteTestTimeout.default

  def createEntity(file: File): Future[RequestEntity] = {
    println(file.getAbsoluteFile)
    val formData =
      Multipart.FormData(
        Source.single(
          Multipart.FormData.BodyPart(
            "test",
            HttpEntity.fromPath(MediaTypes.`application/octet-stream`, file.toPath),
            Map("filename" -> file.getName)))).toStrict(1.minute)
    Marshal(formData).to[RequestEntity]
  }

  "Service" should
    "update file" in {
    val a = createEntity(new File("src/main/resources/Archive.zip")).map(entity => {
      Post(s"/uploadzip", entity) ~> routes ~> check {
        status shouldBe OK
        responseAs[String] should (include("html"))
      }
    })
    Await.result(a, 100.seconds)
  }
}
