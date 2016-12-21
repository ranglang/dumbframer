/* TypeScript importer for Scala.js
 * Copyright 2013-2014 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package importer.sc

import importer.ParseResult
import importer.Trees._

import scala.annotation.tailrec
import scala.collection.mutable

object Printer {
  private implicit val self = this

  final def params2CssString(list: List[TermTree], head: String): String =
    //添加默认值;
    list.foldLeft(head+"position: absolute;\n")((result, term) =>
      term match {
        case HeightIdent(v@StringIdent(s)) =>
          result + "height: " + s + "px;\n"
        case WidthIdent(v@ValueWithIdent(Ident("Screen"), value)) =>
          result + "width: 100%;\n"
        case HeightIdent(v@ValueWithIdent(Ident("Screen"), value)) =>
          result + "height: 100%;\n"
        case WidthIdent(v@StringIdent(s)) =>
          result + "width: " + s + "px;\n"
        case ScrollVerticalIdent(BooleanValueIdent(b)) =>
          result
        case BorderWidthIdent(value@StringIdent(px)) =>
          result + "border-width: " + px + "px;\n"
        case BackGroundColorIdent(color) =>
          result + "background-color: " + color + ";\n"
        case YIdent(v@StringIdent(px)) =>
          result + "top: " + px + "px;" + "\n"
        case XIdent(v@StringIdent(px)) =>
          result + "left: " + px + "px;" + "\n"
        case VisibleIdent(isVisible) if isVisible == false =>
          result + "display: " + "hidden;\n"
        case BorderRadiusIdent(v@StringIdent(value)) =>
          result + "border-radius: " + value + "px;\n"
        case StyleTextAlignIdent(v) =>
          result + "text-align: " + v + ";\n"
        case LineHeightIdent(v) =>
          result + "line-height: " + v + ";\n"
        case StyleFontSizeIdent(v) =>
          result + "font-size: " + v + ";\n"
        case FontColorIdent(v) =>
          result + "color: " + v + ";\n"
        case _ =>
          result;
      })

  final def printSymbol(initialSym: Symbol): ParseResult = {
    @tailrec def factorialAcc(current: Int, hashMap: mutable.HashMap[Int, mutable.Stack[Symbol]], parseResult: ParseResult): ParseResult = {
      if (current.equals(0)) {
        ParseResult(parseResult.html, parseResult.css)
      }
      else {
        println("##### "+current)
        hashMap(current).length match {
          case a: Int if a == 0 && current > 1 =>
            println("a==0")
            factorialAcc(current - 1, hashMap, ParseResult(parseResult.html +"</div>" , parseResult.css))//+ "</div>"
          case a: Int if a == 1 =>
            println("a==1")
            val a = hashMap(current).pop()
            a match {
              case layer: PackageSymbol =>
                println("layer.members: " + layer.members );
                println("layer.members: ");
                println(layer.members);
                layer.members.isEmpty match {
                  case true =>
                    factorialAcc(current - 1, hashMap, ParseResult(parseResult.html , parseResult.css))//+ "<div>" + layer.name.name + "</div>"
                  case false =>
                    val s = mutable.Stack(layer.members: _*)
                    val nh = hashMap.+=((current + 1, s))
                    factorialAcc(current + 1, nh, ParseResult(parseResult.html+"<div>" , parseResult.css))// +  + layer.name.name
                }
              case layer: TextSymbol =>
                val parent = layer.parentOpt.map(s => "." + s + " > ").getOrElse("")
                factorialAcc(current - 1, hashMap, ParseResult(parseResult.html + "<a class=\"" + layer.name.name + "\">" + layer.value + "</a></div>",
                  params2CssString(layer.params, parseResult.css + parent + " ." ++ layer.name.name + "{" + "\n") + "}\n"))
              case layer: LayerSymbol =>
                val parent = layer.parentOpt.map(s => "." + s + " > ").getOrElse("")
                layer.members.isEmpty match {
                  case true =>
                    factorialAcc(current - 1, hashMap, ParseResult(parseResult.html + "<div class=\"" + layer.name.name + "\"></div></div>",
                      params2CssString(layer.params, parseResult.css + parent + "." ++ layer.name.name + "{" + "\n") + "}\n"))
                  case false =>
                    val s = mutable.Stack(layer.members: _*)
                    val nh = hashMap.+=((current + 1, s))
                    factorialAcc(current + 1, nh, ParseResult(parseResult.html + "<div class=\"" + layer.name.name + "\">",
                      params2CssString(layer.params, parseResult.css + parent + "." ++ layer.name.name + "{" + "\n") + "}\n"))
                }
              case sy: Symbol =>
                factorialAcc(current - 1, hashMap, ParseResult(parseResult.html + "<div class=\"" + sy.name.name + "\"></div>", parseResult.css))
            }
          case a: Int if a > 1 =>
            println("a>1")
            val layer = hashMap(current).pop()
            layer match {
              case layer: TextSymbol =>
                val parent = layer.parentOpt.map(s => "." + s + " > ").getOrElse("")
                factorialAcc(current, hashMap, ParseResult(parseResult.html + "<a class=\"" + layer.name.name + "\">" + layer.value + "</a>",
                  params2CssString(layer.params, parseResult.css + parent + " ." ++ layer.name.name + "{" + "\n") + "}\n"))
              case layer: LayerSymbol =>
                val parent = layer.parentOpt.map(s => "." + s + " > ").getOrElse("")
                layer.members.isEmpty match {
                  case true =>
                    factorialAcc(current, hashMap, ParseResult(parseResult.html + "<div class=\"" + layer.name.name + "\"></div>",
                      params2CssString(layer.params, parseResult.css + parent + "." ++ layer.name.name + "{" + "\n") + "}\n"))
                  case false =>
                    val s = mutable.Stack(layer.members: _*)
                    val nh = hashMap.+=((current + 1, s))
                    factorialAcc(current + 1, nh, ParseResult(parseResult.html + "<div class=\"" + layer.name.name + "\">",
                      params2CssString(layer.params, parseResult.css + parent + "." ++ layer.name.name + "{" + "\n") + "}\n"))
                }
              case layer: PageSymbol =>
                factorialAcc(current, hashMap, ParseResult(parseResult.html + "<div class=\"" + layer.name.name + "\">",
                  params2CssString(layer.params, parseResult.css + "." ++ layer.name.name + "{" + "\n") + "}\n"))
              case symbol: Symbol =>
                factorialAcc(current, hashMap, parseResult)
            }
          case _ =>
            parseResult
        }
      }
    }

    val initialStack: mutable.Stack[Symbol] = mutable.Stack(initialSym)
    val hm: mutable.HashMap[Int, mutable.Stack[Symbol]] = new mutable.HashMap[Int, mutable.Stack[Symbol]]()
    hm.put(1, initialStack)
    factorialAcc(1, hm, ParseResult("", "")) //免添加 <div>
  }
}

