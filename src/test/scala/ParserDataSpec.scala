import org.scalatest.FlatSpec
import akka.stream.scaladsl.Flow
import importer.Importer
import importer.Trees._
import importer.sc._
import org.scalatest._

import scala.collection.mutable.ListBuffer
/**
  * Created by tian on 16/12/2016.
  */
class ParserDataSpec extends  FlatSpec with Matchers{

  "1" should "printer should has header" in {
    Printer.printTab(2) shouldBe "\t\t"
  }

  "1" should "printer should has  three header" in {
    Printer.printTab(3) shouldBe "\t\t\t"
  }

  "1" should "11" in {
    Printer.transformUrl("images/1.png","") shouldBe "1.png"
  }

  "PrintSymbol" should "respond should be single" in {
    val packageSymbol = PackageSymbol(Name("package"))
    packageSymbol.members += LayerSymbol(Name("A"),List.empty[TermTree], ListBuffer(), Some("package"))
    val parserResult = Printer.printSymbol(packageSymbol,Option.empty[String])
       val a  = "\t<div>\n" +
       "\t\t<div class=\"A\"></div>\n" +
       "\t</div>\n"
    parserResult.html shouldBe a
  }
//
  "PrintSymbol" should "respond should be single1" in {
    val packageSymbol = PackageSymbol(Name("package"))
    val b = LayerSymbol(Name("B"),List.empty[TermTree], ListBuffer(), Some("A"))
    val c = LayerSymbol(Name("C"),List.empty[TermTree], ListBuffer(), Some("A"))
    packageSymbol.members += LayerSymbol(Name("A"),List.empty[TermTree], ListBuffer(b,c), Some("package"))
    val parserResult = Printer.printSymbol(packageSymbol,Option.empty[String])
    val a  = "\t<div>\n" +
      "\t\t<div class=\"A\">\n" +
    "\t\t\t<div class=\"B\"></div>\n"+
    "\t\t\t<div class=\"C\"></div>\n"+
    "\t\t</div>\n" +
      "\t</div>\n"
    parserResult.html shouldBe  a
  }

}
