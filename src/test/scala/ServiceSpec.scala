import java.io.File

import akka.event.NoLogging
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model.ContentTypes._
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Flow, Source}
import com.qiniu.common.Zone
import com.qiniu.storage.{Configuration, UploadManager}
import com.qiniu.util.Auth
import org.scalatest._

import scala.concurrent.Future

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
//  override implicit val materializer = ActorMaterializer()
//  override implicit val executor = system.dispatcher

  def createEntity(file: File): Future[RequestEntity] = {
    println(file.getAbsoluteFile)
    val formData =
      Multipart.FormData(
        Source.single(
          Multipart.FormData.BodyPart(
            "test",
            HttpEntity.fromPath(MediaTypes.`application/octet-stream`, file.toPath, 100000),
            Map("filename" -> file.getName))))
    Marshal(formData).to[RequestEntity]
  }

  "Service" should
    "update file" in {
      createEntity(new File("src/main/resources/Archive.zip")).map(entity => {
        println("..")
        println(entity)
        Post(s"/uploadzip", entity) ~> routes ~> check {
          status shouldBe OK
        }
      })
  }

  "Service" should "return string ip" in {
    Get(s"/ip") ~> routes ~> check {
      status shouldBe OK
      responseAs[String] shouldBe "ip"
    }

  }
}
