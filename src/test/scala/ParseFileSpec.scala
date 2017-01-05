
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

class ParseFileSpec
  extends TestKit(ActorSystem(
    "TestKitUsageSpec"
  ))
    with DefaultTimeout
    with ImplicitSender
    with WordSpecLike
    with Matchers
    with BeforeAndAfterAll with Protocols {

  implicit val materializer = ActorMaterializer()

//  "FramerCongfig" should {
//    "should paser style has lineHeight" in {
//      val reader = PagedSeq.fromReader(new InputStreamReader(new FileInputStream("src/main/resources/line_height_coffeeScript")))
//      val c = new PagedSeqReader(reader);
//      val result = FramerParser.parse(c, FramerConfig("apple-iphone-5s-gold", ""))
//      println(result)
//      result.css should (include("line-height"))
//    }
//  }
//
//
//  "FramerCongfig" should {
//    "should paser file" in {
//      val reader = PagedSeq.fromReader(new InputStreamReader(new FileInputStream("src/main/resources/test_coffeeScript")))
//      val c = new PagedSeqReader(reader);
//      val result = FramerParser.parse(c, FramerConfig("apple-iphone-5s-gold", ""))
//      result.css.contains("display: flex") shouldBe true
//    }
//
//    "should paser file right" in {
//      val reader = PagedSeq.fromReader(new InputStreamReader(new FileInputStream("src/main/resources/testRight.coffee")))
//      val c = new PagedSeqReader(reader);
//      val result = FramerParser.parse(c, FramerConfig("apple-iphone-5s-gold", ""))
//      println(result.css)
//      result.css.contains("display: flex") shouldBe true
//    }
//  }
//
//  "FramerCongfig" should {
//    "should has iphone 5s gold" in {
//      val framerConfigFile = new File("src/main/resources/config.json")
//      val lines: String = FileUtils.readFileToString(framerConfigFile, "UTF-8");
//      val a = JsonParser(lines)
//      val b = a.convertTo[FramerConfig]
//
//      b shouldBe FramerConfig(deviceType = "apple-iphone-5s-gold", "5D61D5B4-85D6-4EE7-9C3B-160981C8DAE4")
//    }
//  }
}
