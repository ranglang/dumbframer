
import java.io.{File, FileInputStream, InputStreamReader}

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer

import scala.concurrent.duration._
import akka.stream.scaladsl.{FileIO, Source}
import akka.testkit.{DefaultTimeout, ImplicitSender, TestKit}
import akka.util.ByteString
import com.typesafe.config.ConfigFactory
import importer.FramerParser
import org.apache.commons.io.FileUtils
import org.scalatest._
import org.scalatest.FlatSpec
import spray.json.JsonParser
import utl.{FramerConfig, Unzip}

import scala.collection.immutable.PagedSeq
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.util.parsing.input.PagedSeqReader

/**
  * Created by tian on 23/12/2016.
  */



//extends  FlatSpec with Matchers with Protocols{
class ParseFileSpec
  extends TestKit(ActorSystem(
    "TestKitUsageSpec" //,
    //      ConfigFactory.parseString(TestKitUsageSpec.config)
  ))
        with DefaultTimeout
        with ImplicitSender
        with WordSpecLike
        with Matchers
        with BeforeAndAfterAll {

  implicit val materializer = ActorMaterializer()


  val TestLines = {
    val b = ListBuffer[String]()
    b.append("a" * 1000 + "\n")
    b.append("b" * 1000 + "\n")
    b.append("c" * 1000 + "\n")
    b.append("d" * 1000 + "\n")
    b.append("e" * 1000 + "\n")
    b.append("f" * 1000 + "\n")
    b.toList
  }

  //  "Framer unzip" should "should unzip file1" in {
  //  }
  "FileSink" should {
    "a" in {
      val TestByteStrings = TestLines.map(ByteString(_))
      val q:Source[ByteString,NotUsed] = Source(TestByteStrings)
       val completion = q.runWith(FileIO.toPath(new File("1.md").toPath))
      val result = Await.result(completion, 3.seconds)
      result.count should equal(6006)
      //        checkFileContents(f, TestLines.mkString(""))
    }
  }


    "Framer unzip" should "should unzip file" in {
      val file =  new File("src/main/resources/Archive.zip")
      Unzip.unzip(new FileInputStream("src/main/resources/Archive.zip"),new File("src/a").toPath)
      1 shouldBe 1
    }

    "FramerCongfig" should "should paser file" in {
      val reader = PagedSeq.fromReader(new InputStreamReader(new FileInputStream("src/main/resources/test_coffeeScript")))
        val c = new PagedSeqReader(reader);
        val result = FramerParser.parse(c,FramerConfig("apple-iphone-5s-gold",""))
        result.css shouldBe "";
        result.html shouldBe "";
    }

    "FramerCongfig" should "should has iphone 5s gold" in {
      val framerConfigFile = new File( "src/main/resources/config.json")
      val lines:String = FileUtils.readFileToString(framerConfigFile, "UTF-8");
      val a =  JsonParser(lines)
      val b = a.convertTo[FramerConfig]

      b shouldBe FramerConfig(deviceType = "apple-iphone-5s-gold", "5D61D5B4-85D6-4EE7-9C3B-160981C8DAE4")
    }
}
