
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
  "FramerCongfig" should {
//    "should paser test one layer" in {
//      val reader = PagedSeq.fromReader(new InputStreamReader(new FileInputStream("src/main/resources/testonelayer.coffee")))
//      val c = new PagedSeqReader(reader);
//      val result = FramerParser.parse(c, FramerConfig("apple-iphone-5s-gold", ""))
////      result.html.split("div") should (have(length(5)))
//      result.html shouldBe ""
//    }

    "should paser test multi layer" in {
            val reader = PagedSeq.fromReader(new InputStreamReader(new FileInputStream("src/main/resources/testmultilayer.coffee")))
            val c = new PagedSeqReader(reader);
            val result = FramerParser.parse(c, FramerConfig("apple-iphone-5s-gold", ""))
      val result1 = FramerParser.parseVnode(c, FramerConfig("apple-iphone-5s-gold", ""))
//      result.html shouldBe ""
      result1.html shouldBe ""
    }

    "should paser file" in {
      val reader = PagedSeq.fromReader(new InputStreamReader(new FileInputStream("src/main/resources/test_coffeeScript")))
      val c = new PagedSeqReader(reader);
      val result = FramerParser.parse(c, FramerConfig("apple-iphone-5s-gold", ""))
      result.css.contains("display: flex") shouldBe true
    }
    "should paser file2" in {
      val reader = PagedSeq.fromReader(new InputStreamReader(new FileInputStream("src/main/resources/testMargin.coffee")))
      val c = new PagedSeqReader(reader);
      val result = FramerParser.parse(c, FramerConfig("apple-iphone-5s-gold", ""))
      result.css.contains("margin-right") shouldBe true

      val result1 = FramerParser.parseVnode(c, FramerConfig("apple-iphone-5s-gold", ""))
      result1.html should (include("marginRight"))// shouldBe true
    }

    "should test textAlign" in {
      val reader = PagedSeq.fromReader(new InputStreamReader(new FileInputStream("src/main/resources/testTextAlign.coffee")))
      val c = new PagedSeqReader(reader);
      val result = FramerParser.parse(c, FramerConfig("apple-iphone-5s-gold", ""))
      result.css should  (include("text-align"))

      val result_vnode = FramerParser.parseVnode(c, FramerConfig("apple-iphone-5s-gold", ""))
      result_vnode.html should (include ("textAlign"))

    }

    "should include image" in {
      val reader = PagedSeq.fromReader(new InputStreamReader(new FileInputStream("src/main/resources/testImageSrc.coffee")))
      val c = new PagedSeqReader(reader);
      val result = FramerParser.parseVnode(c, FramerConfig("apple-iphone-5s-gold", ""))
      result.html should  (include("prop"))
      result.html should  (include("src"))
    }

    "should include textAlign" in {
      val reader = PagedSeq.fromReader(new InputStreamReader(new FileInputStream("src/main/resources/testTextAlign1.coffee")))
      val c = new PagedSeqReader(reader);
      val result = FramerParser.parse(c, FramerConfig("apple-iphone-5s-gold", ""))
      result.css should  (include("text-align"))

    }
  }
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
