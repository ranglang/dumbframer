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
    list.foldLeft(head)((result,term ) =>
      term match {
        case HeightIdent(v @ StringIdent(s)) =>
          result +"height: " +s+"px;\n"
        case WidthIdent(v @ StringIdent(s)) =>
          result +"width: " +s+"px;\n"
        case ScrollVerticalIdent(BooleanValueIdent(b)) =>
          result
        case BorderWidthIdent(value @ StringIdent(px))=>
          result +"border-width: " +px+"px;\n"
        case BackGroundColorIdent(color) =>
          result +"background-color: " +color+"\n"
        case XIdent(v @ StringIdent(px)) =>
          result +"x: " +px+"px;"+"\n"
        case VisibleIdent(isVisible) if isVisible == false =>
          result +"display: "+"hidden;\n"
        case BorderRadiusIdent(v @ StringIdent(value)) =>
          result +"border-radius: " +value+"px;\n"
        case StyleTextAlignIdent(v) =>
          result +"text-align: " +v+";\n"
        case TextHeightIdent(v) =>
          result +"text-height: " +v+";\n"
        case StyleFontSizeIdent(v) =>
          result +"font-size: " +v+";\n"
        case _ =>
          result;
      })

  final def printSymbol(initialSym: Symbol): ParseResult = {

    @tailrec def factorialAcc(current: Int, hashMap: mutable.HashMap[Int, mutable.Stack[Symbol]], parseResult: ParseResult): ParseResult = {

      if (current.equals(0)) {
        parseResult
      }
      else {
        hashMap(current).length match {
          case a: Int if a == 0 =>
            factorialAcc(current - 1, hashMap, ParseResult (parseResult.html+"</div>",parseResult.css))
          case a: Int if a == 1 =>
            val a = hashMap(current).pop()
            a match {
              case layer: PackageSymbol =>
                println("PackageSymbol ")
                println(layer.members.length)
                println(layer.members)
                layer.members.isEmpty match {
                  case true =>
                    factorialAcc(current - 1, hashMap, ParseResult(parseResult.html + "<div>" + layer.name.name + "</div></div>", parseResult.css))
                  case false =>
                    val s = mutable.Stack(layer.members: _*)
                    val nh = hashMap.+=((current + 1, s))
                    factorialAcc(current + 1, nh, ParseResult(parseResult.html + "<div>" + layer.name.name,parseResult.css))
                }
              case layer: TextSymbol =>
                println("TextSymbol: a ==1")
                val parent = layer.parentOpt.map(s => "."+s).getOrElse("")
                factorialAcc(current - 1, hashMap, ParseResult(parseResult.html + "<a class=\"" + layer.name.name +"\">"+layer.value+"</a></div>",
                params2CssString(layer.params, parseResult.css+parent+ "."++ layer.name.name +"{"+"\n")+ "}"))
              case layer: LayerSymbol =>
                println("layer: a ==1")
                val parent = layer.parentOpt.map(s => "."+s).getOrElse("")
                layer.members.isEmpty match {
                  case true =>
                    factorialAcc(current - 1, hashMap, ParseResult(parseResult.html + "<div class=\"" + layer.name.name +"\"></div></div>",
                      params2CssString(layer.params, parseResult.css+parent+ "."++ layer.name.name +"{"+"\n")+ "}"))
                  case false =>
                    val s = mutable.Stack(layer.members: _*)
                    val nh = hashMap.+=((current + 1, s))
                    factorialAcc(current + 1, nh, ParseResult(parseResult.html + "<div class=\"" + layer.name.name +"\">",
                      params2CssString(layer.params, parseResult.css + parent+ "."++ layer.name.name +"{"+"\n")+ "}"))
                }
              case sy: CommentSymbol =>
                factorialAcc(current - 1, hashMap, parseResult)
              case sy: Symbol =>
                factorialAcc(current - 1, hashMap, ParseResult(parseResult.html +"<div class=\"" + sy.name.name +"\"></div></div>",parseResult.css))
            }
          case a: Int if a > 1 =>
            val layer = hashMap(current).pop()
            layer match {
              case comment: CommentSymbol =>
                factorialAcc(current, hashMap, ParseResult(parseResult.html +"<div class=\"" + comment.name.name +"\"></div></div>",parseResult.css))
              case layer: LayerSymbol =>
                println("layer a>1")
                val parent = layer.parentOpt.map(s => "."+s).getOrElse("")
                layer.members.isEmpty match {
                  case true =>
                    factorialAcc(current , hashMap, ParseResult(parseResult.html + "<div class=\"" + layer.name.name +"\">",
                      params2CssString(layer.params, parseResult.css+parent+ "."++ layer.name.name +"{"+"\n")+ "}"))
                  case false =>
                    val s = mutable.Stack(layer.members: _*)
                    val nh = hashMap.+=((current + 1, s))
                    factorialAcc(current +1, nh, ParseResult(parseResult.html + "<div class=\"" + layer.name.name +"\">",
                      params2CssString(layer.params, parseResult.css+parent+ "."++ layer.name.name +"{"+"\n")+ "}"))
                }

              case layer: PageSymbol =>
                factorialAcc(current , hashMap, ParseResult(parseResult.html + "<div class=\"" + layer.name.name +"\">",
                  params2CssString(layer.params, parseResult.css+ "."++ layer.name.name +"{"+"\n")+ "}"))
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
//    println(initialSym);
    factorialAcc(1, hm, ParseResult("<div>",""))
  }
}

