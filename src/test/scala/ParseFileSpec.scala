
import java.io.{File, FileInputStream, InputStreamReader}

import importer.FramerParser
import org.apache.commons.io.FileUtils
import org.scalatest._
import org.scalatest.FlatSpec
import spray.json.JsonParser
import utl.FramerConfig

import scala.collection.immutable.PagedSeq
import scala.util.parsing.input.PagedSeqReader
/**
  * Created by tian on 23/12/2016.
  */


class ParseFileSpec extends  FlatSpec with Matchers with Protocols{

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
