
import java.io.File

import org.apache.commons.io.FileUtils
import org.scalatest._
import org.scalatest.FlatSpec
import spray.json.JsonParser
import utl.FramerConfig
/**
  * Created by tian on 23/12/2016.
  */
class ParseFileSpec extends  FlatSpec with Matchers with Protocols{

  "FramerCongfig" should "should has iphone 5s gold" in {
    val framerConfigFile = new File( "src/main/resources/config.json")
    val lines:String = FileUtils.readFileToString(framerConfigFile, "UTF-8");
    val a =  JsonParser(lines)
    val b = a.convertTo[FramerConfig]

    b shouldBe FramerConfig(deviceType = "apple-iphone-5s-gold", "5D61D5B4-85D6-4EE7-9C3B-160981C8DAE4")
  }
}
