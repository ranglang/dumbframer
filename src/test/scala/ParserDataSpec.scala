import org.scalatest.FlatSpec
import akka.stream.scaladsl.Flow
import importer.Importer
import importer.Trees._
import importer.sc._
import org.scalatest._
import utl.FramerConfig

import scala.collection.mutable.ListBuffer
/**
  * Created by tian on 16/12/2016.
  */
class ParserDataSpec extends  FlatSpec with Matchers {

  "TabIndent" should "should has 2 tabs" in {
    Printer.printTab(2) shouldBe "\t\t"
  }

  "Printer" should "transformUrl" in {
    Printer.transformUrl("images/1.png","") shouldBe "1.png"
  }

  "PrintSymbol" should "should recognize class" in {
    val packageSymbol = PackageSymbol(Name("package"))
    packageSymbol.members +=  PageSymbol(Name("pageScroller"),List.empty[TermTree],ListBuffer(
     LayerSymbol(Name("B"),List(ParentIdent(Ident("pageScroller"))), ListBuffer(), Some("pageScroller"))
    ))
    val parserResult = Printer.printSymbol(packageSymbol,Option.empty[String],FramerConfig("apple-iphone-5s-gold",""))
    parserResult.html shouldBe "\t<div>\n\t\t<div class=\"pageScroller\"></div>\n"
  }

  "PrintSymbol" should "respond should be single" in {
    val packageSymbol = PackageSymbol(Name("package"))
    packageSymbol.members += LayerSymbol(Name("A"),List.empty[TermTree], ListBuffer(), Some("package"))
    val parserResult = Printer.printSymbol(packageSymbol,Option.empty[String],FramerConfig("apple-iphone-5s-gold",""))
       val a  = "\t<div>\n" +
       "\t\t<div class=\"A\"></div>\n" +
       "\t</div>\n"
    parserResult.html shouldBe a
  }

  "PrintSymbol" should "respond should be subclass" in {
    val packageSymbol = PackageSymbol(Name("package"))
    val b = LayerSymbol(Name("B"),List.empty[TermTree], ListBuffer(), Some("A"))
    val c = LayerSymbol(Name("C"),List.empty[TermTree], ListBuffer(), Some("A"))
    packageSymbol.members += LayerSymbol(Name("A"),List.empty[TermTree], ListBuffer(b,c), Some("package"))
    val parserResult = Printer.printSymbol(packageSymbol,Option.empty[String],FramerConfig("apple-iphone-5s-gold",""))
    val a  = "\t<div>\n" +
      "\t\t<div class=\"A\">\n" +
    "\t\t\t<div class=\"B\"></div>\n"+
    "\t\t\t<div class=\"C\"></div>\n"+
    "\t\t</div>\n" +
      "\t</div>\n"
    parserResult.html shouldBe  a
  }
}
