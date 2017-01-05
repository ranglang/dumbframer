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
class ParserDataSpec extends  FlatSpec with Matchers{


  "1" should "printer should has  three header" in {
    Printer.printTab(3) shouldBe "\t\t\t"
  }

  "1" should "11" in {
    Printer.transformUrl("images/1.png","") shouldBe "1.png"
  }


  "PrintSymbol" should "hello" in {
    val packageSymbol = PackageSymbol(Name("package"))
    packageSymbol.members +=  PageSymbol(Name("pageScroller"),List.empty[TermTree],ListBuffer(
      LayerSymbol(Name("B"),List(ParentIdent(Ident("pageScroller"))), ListBuffer(), Some("pageScroller"))
    ))
  }

  "PrintSymbol" should "vnode" in {
    val packageSymbol = PackageSymbol(Name("package"))
    packageSymbol.members += LayerSymbol(Name("B"),List(
      WidthIdent(StringIdent("3")),
      HeightIdent(StringIdent("4")),
      BackGroundColorIdent("red"),
      ParentIdent(Ident("pageScroller"))), ListBuffer(), Some("package"))
    val parserResult = Printer.printSymbolVNode(packageSymbol,FramerConfig(deviceType = "apple-iphone-5s-gold", "5D61D5B4-85D6-4EE7-9C3B-160981C8DAE4"))
    println(parserResult.html)
     parserResult.html shouldBe ""
  }


}
