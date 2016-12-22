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

  "1" should "11" in {
    Printer.transformUrl("images/1.png","") shouldBe "1.png"
  }
  "PrintSymbol" should "respond should be single" in {
    val packageSymbol = PackageSymbol(Name("package"))
    packageSymbol.members += LayerSymbol(Name("A"),List.empty[TermTree], ListBuffer(), Some("package"))
    val parserResult = Printer.printSymbol(packageSymbol,Option.empty[String])
    println(parserResult.html)
     parserResult.html shouldBe "<div><div class=\"A\"></div></div>"
//    parserResult.css should be (".A{"+"\n"+"}")
  }

  "PrintSymbol" should "respond should be single1" in {
    val packageSymbol = PackageSymbol(Name("package"))
    val b = LayerSymbol(Name("B"),List.empty[TermTree], ListBuffer(), Some("A"))
    val c = LayerSymbol(Name("C"),List.empty[TermTree], ListBuffer(), Some("A"))
    packageSymbol.members += LayerSymbol(Name("A"),List.empty[TermTree], ListBuffer(b,c), Some("package"))
    val parserResult = Printer.printSymbol(packageSymbol,Option.empty[String])
    println(parserResult.html)
//    <div>
//      <div class="A">
//        <div class="B"></div>
//        <div class="C"></div>
//      </div>
//    </div>
    parserResult.html shouldBe "<div><div class=\"A\"><div class=\"B\"></div><div class=\"C\"></div></div></div>"
  }


  "PrintSymbol" should "respond should be single2" in {
    val packageSymbol = PackageSymbol(Name("package"))

    val thirdA = LayerSymbol(Name("E"),List.empty[TermTree], ListBuffer(), Some("B"))

    val b = LayerSymbol(Name("B"),List.empty[TermTree], ListBuffer(thirdA), Some("A"))
    val c = LayerSymbol(Name("C"),List.empty[TermTree], ListBuffer(), Some("A"))
    packageSymbol.members += LayerSymbol(Name("A"),List.empty[TermTree], ListBuffer(b,c), Some("package"))
    val parserResult = Printer.printSymbol(packageSymbol,Option.empty[String])
    println(parserResult.html)
    parserResult.html shouldBe "<div><div class=\"A\"><div class=\"B\"></div><div class=\"C\"></div></div></div>"
  }

  "PrintSymbol" should "respond should be single3" in {
    val packageSymbol = PackageSymbol(Name("package"))

    val thirdA = LayerSymbol(Name("E"),List.empty[TermTree], ListBuffer(), Some("B"))
    val thirdB = LayerSymbol(Name("F"),List.empty[TermTree], ListBuffer(), Some("B"))

    val b = LayerSymbol(Name("B"),List.empty[TermTree], ListBuffer(thirdA,thirdB), Some("A"))
    val c = LayerSymbol(Name("C"),List.empty[TermTree], ListBuffer(), Some("A"))
    packageSymbol.members += LayerSymbol(Name("A"),List.empty[TermTree], ListBuffer(b,c), Some("package"))
    val parserResult = Printer.printSymbol(packageSymbol,Option.empty[String])
    println(parserResult.html)
    parserResult.html shouldBe "<div><div class=\"A\"><div class=\"B\"></div><div class=\"C\"></div></div></div>"
  }

//
//  "TermTree" should "respond should has width and height" in {
//    val s =  Printer.params2CssString(List(new WidthIdent(StringIdent("7")), new HeightIdent(StringIdent("8"))),"")
//    s should be ("width: 7px;\nheight: 8px;\n")
//  }
//
//  "TermTree" should "respond should be has be x :7px; and some on" in {
//   val s =  Printer.params2CssString(List(new XIdent(StringIdent("7")), new VisibleIdent(true), new BorderRadiusIdent(StringIdent("7")),
//     new BackGroundColorIdent("black"), new BorderWidthIdent(StringIdent("10"))
//   ),"")
//    s should be ("x: 7px;\nborder-radius: 7px;\nbackground-color: black\nborder-width: 10px;\n")
//  }
//
//  "TermTree" should "respond should be has be x :7px; and should be hidden" in {
//    Printer.params2CssString(List(new XIdent(StringIdent("7")), new VisibleIdent(false)),"") should be ("x: 7px;\ndisplay: hidden;\n")
//  }
//
//  "PrintSymbol" should "respond should has css" in {
//    val parserResult = Printer.printSymbol(LayerSymbol(Name("A"),List(WidthIdent(StringIdent("7"))),ListBuffer(),Option.empty[String]))
//    parserResult.html should be ("<div><div class=\"A\"></div></div>")
//    parserResult.css should be (".A{"+"\nwidth: 7px;"++"\n}")
//  }
//
//  "PrintSymbol" should "respond should has parent" in {
//    val parserResult = Printer.printSymbol(LayerSymbol(Name("A"),List(WidthIdent(StringIdent("7"))),ListBuffer(), Some("B")))
//    parserResult.html should be ("<div><div class=\"A\"></div></div>")
//    parserResult.css should be (".B.A{"+"\nwidth: 7px;"++"\n}")
//  }
//
//  "TextSymbol" should "respond should not has <a>" in {
//    val parserResult = Printer.printSymbol(TextSymbol(Name("A"),"hello",List(
//      WidthIdent(ValueWithIdent(Ident("Screen"),"width")),
//      TextHeightIdent("23px")), None))
//    parserResult.html should be ("<div><a class=\"A\">hello</a></div>")
//    println(parserResult.css)
//    parserResult.css should be (".A{"+"\nwidth: 100%;\nfont-size: 74px;"+"\ntext-height: 23px;"+"\n}")
//  }
//
//  "PrintSymbol" should "respond should have multic symbol" in {
//    val a = PackageSymbol(Name.EMPTY)
//      a.members += LayerSymbol(Name("Farther"),List(WidthIdent(StringIdent("7"))),ListBuffer(
//       LayerSymbol(Name("A"),List(WidthIdent(StringIdent("7"))),ListBuffer(), Some("Farther")),
//      LayerSymbol(Name("B"),List(WidthIdent(StringIdent("7"))),ListBuffer(), Some("Farther"))
//    ), Option.empty[String])
//
//    val parserResult = Printer.printSymbol(a)
//    println(parserResult.css)
//    parserResult.html should be ("<div><div><div class=\"Farther\"><div class=\"A\"><div class=\"B\"></div></div></div></div>")
//    parserResult.css should be (".Farther{\nwidth: 7px;\n}.Farther.A{\nwidth: 7px;\n}.Farther.B{\nwidth: 7px;\n}")
//  }
}
