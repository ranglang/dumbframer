import org.scalatest.FlatSpec
import akka.stream.scaladsl.Flow
import importer.Importer
import importer.Trees._
import importer.sc.{LayerSymbol, Name, PackageSymbol, Printer}
import org.scalatest._

import scala.collection.mutable.ListBuffer
/**
  * Created by tian on 16/12/2016.
  */
class ParserDataSpec extends  FlatSpec with Matchers{

  "PrintSymbol" should "respond should be single" in {
    val parserResult = Printer.printSymbol(LayerSymbol(Name("A"),List.empty[TermTree],ListBuffer(),Option.empty[String]))
    parserResult.html should be ("<div><div class=\"A\"></div></div>")
    parserResult.css should be (".A{"+"\n"+"}")
  }

  "PrintSymbol" should "respond should be has css" in {
    val parserResult = Printer.printSymbol(LayerSymbol(Name("A"), List(new XIdent(StringIdent("7"))),ListBuffer(),Option.empty[String]))
    parserResult.html should be ("<div><div class=\"A\"></div></div>")
    parserResult.css should be (".A{"+"\n"+"x: 7px;"+"\n}")
  }

  "TermTree" should "respond should has width and height" in {
    val s =  Printer.params2CssString(List(new WidthIdent(StringIdent("7")), new HeightIdent(StringIdent("8"))),"")
    s should be ("width: 7px;\nheight: 8px;\n")
  }

  "TermTree" should "respond should be has be x :7px; and some on" in {

   val s =  Printer.params2CssString(List(new XIdent(StringIdent("7")), new VisibleIdent(true), new BorderRadiusIdent(StringIdent("7")),
     new BackGroundColorIdent("black"), new BorderWidthIdent(StringIdent("10"))
   ),"")
    s should be ("x: 7px;\nborder-radius: 7px;\nbackground-color: black\nborder-width: 10px;\n")
  }

  "TermTree" should "respond should be has be x :7px; and should be hidden" in {
    Printer.params2CssString(List(new XIdent(StringIdent("7")), new VisibleIdent(false)),"") should be ("x: 7px;\ndisplay: hidden;\n")
  }

  "PrintSymbol" should "respond should has css" in {
    val parserResult = Printer.printSymbol(LayerSymbol(Name("A"),List(WidthIdent(StringIdent("7"))),ListBuffer(),Option.empty[String]))
    parserResult.html should be ("<div><div class=\"A\"></div></div>")
    parserResult.css should be (".A{"+"\nwidth: 7px;"++"\n}")
  }

  "PrintSymbol" should "respond should has parent" in {
    val parserResult = Printer.printSymbol(LayerSymbol(Name("A"),List(WidthIdent(StringIdent("7"))),ListBuffer(), Some("B")))
    parserResult.html should be ("<div><div class=\"A\"></div></div>")
    parserResult.css should be (".B.A{"+"\nwidth: 7px;"++"\n}")
  }

  "PrintSymbol" should "respond should have multic symbol" in {
    val a = PackageSymbol(Name.EMPTY)
      a.members += LayerSymbol(Name("Farther"),List(WidthIdent(StringIdent("7"))),ListBuffer(
       LayerSymbol(Name("A"),List(WidthIdent(StringIdent("7"))),ListBuffer(), Some("Farther")),
      LayerSymbol(Name("B"),List(WidthIdent(StringIdent("7"))),ListBuffer(), Some("Farther"))
    ), Option.empty[String])

    val parserResult = Printer.printSymbol(a)
    println(parserResult.css)
    parserResult.html should be ("<div><div><div class=\"Farther\"><div class=\"A\"><div class=\"B\"></div></div></div></div>")
    parserResult.css should be (".Farther{\nwidth: 7px;\n}.Farther.A{\nwidth: 7px;\n}.Farther.B{\nwidth: 7px;\n}")
  }
}
