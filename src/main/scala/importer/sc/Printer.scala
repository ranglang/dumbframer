/* TypeScript importer for Scala.js
 * Copyright 2013-2014 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package importer.sc

import importer.ParseResult
import importer.Trees._
import utl.FramerConfig

import scala.annotation.tailrec
import scala.collection.mutable

object Printer {
  final def printTab(current: Int) :String  = {
    val  list : List [Int]  = (1 to current).toList
    list.foldLeft("")((result, item) => result +"\t")
  }

  final def transformUrl (a1: String, a2: String ): String = {
    val a = a1.indexOf("/")
    a2 + a1.substring(a +1)
  }
  private implicit val self = this

  final def params2CssString(list1: List[TermTree], head: String)(implicit framer: FramerConfig): String = {
    val ifResponsive = !framer.selectedHand.isEmpty;
    val SCREEN_WIDTH = framer.deviceType match {
      case "apple-iphone-5c-white" => 640
      case _ => 600
    }
    val list2:List[TermTree] = list1.collectFirst {
      case str: WidthIdent => str
    } match {
      case Some(widthIdent) =>  list1
      case None => list1.+:(WidthIdent(StringIdent("100")))
    }

    val list3 = list2.collectFirst {
      case str: HeightIdent => str
    } match {
      case Some(widthIdent) => list2
      case None =>  list2.+:(HeightIdent(StringIdent("100")))
    }

    val list4 = list3.collectFirst {
      case str: XIdent => str
    } match {
      case Some(ident) => list3
      case None =>  list3.+:(XIdent(StringIdent("100")))
    }

    val list = list4.collectFirst {
      case str: YIdent => str
    } match {
      case Some(ident) => list4
      case None =>  list4.+:(YIdent(StringIdent("100")))
    }

    list.foldLeft(head + "display: flex;\nposition: relative;\n")((result, term) =>
      term match {
        case HeightIdent(v@StringIdent(s)) =>
          if(ifResponsive) result + "height: " + s.toDouble / SCREEN_WIDTH  + "rem;\n"
          else result + "height: " + s + "px;\n"
        case WidthIdent(v@ValueWithIdent(Ident("Screen"), value)) =>
          result + "width: 100%;\n"
        case HeightIdent(v@ValueWithIdent(Ident("Screen"), value)) =>
          result + "height: 100%;\n"
        case WidthIdent(v@StringIdent(s)) =>
          if(ifResponsive) result + "width: " + s.toDouble / SCREEN_WIDTH  + "rem;\n" else
          result + "width: " + s + "px;\n"
        case ScrollVerticalIdent(BooleanValueIdent(b)) =>
          result
        case BorderWidthIdent(value@StringIdent(px)) =>
          if(ifResponsive) result + "border-width: " + px.toDouble / SCREEN_WIDTH  + "rem;\n" else
          result + "border-width: " + px + "px;\n"
        case BackGroundColorIdent(color) =>
          result + "background-color: " + color + ";\n"
        case YIdent(v @Value3Ident(pos,optCal,optStr)) =>
          pos match {
            case "center" =>{
               result + "margin-top: auto;\nmargin-bottom: auto;\n"
            }
          }
        case XIdent(v @Value3Ident(pos,optCal,optStr)) =>
          pos match {
            case "center" => {
               result + "margin-left: auto;\nmargin-right: auto;\n"
            }
          }
        case YIdent(v@StringIdent(px)) =>
          if(ifResponsive) result + "top: " + px.toDouble / SCREEN_WIDTH  + "rem;\n" else
          result + "top: " + px + "px;" + "\n"
        case XIdent(v@StringIdent(px)) =>
          if(ifResponsive) result + "left: " + px.toDouble / SCREEN_WIDTH  + "rem;\n" else
          result + "left: " + px + "px;" + "\n"
        case VisibleIdent(isVisible) if isVisible == false =>
          result + "display: " + "hidden;\n"
        case BorderRadiusIdent(v@StringIdent(value)) =>
          if(ifResponsive) result + "border-radius: " + value.toDouble / SCREEN_WIDTH  + "rem;\n" else
          result + "border-radius: " + value + "px;\n"
        case StyleTextAlignIdent(v) =>
          result + "text-align: " + v + ";\n"
        case LineHeightIdent(v) =>
          if(ifResponsive) result + "border-radius: " + v.toDouble / SCREEN_WIDTH  + "rem;\n" else
          result + "line-height: " + v + "px;\n"
        case StyleFontSizeIdent(v) =>
          if(ifResponsive) result + "font-size: " + v.toDouble / SCREEN_WIDTH  + "rem;\n" else
          result + "font-size: " + v + "px;\n"
        case BorderColorIdent(v) =>
          if(ifResponsive) result + "border-color: " + v.toDouble / SCREEN_WIDTH  + ";\n" else
            result + "border-color: " + v +";\n"
        case FontColorIdent(v) =>
          result + "color: " + v + ";\n"
        case _ =>
          result;
      })
  }

  final def printSymbol(initialSym: Symbol, framerConfig: FramerConfig): ParseResult = {
    implicit val framer:FramerConfig = framerConfig;
    @tailrec def factorialAcc(current: Int, hashMap: mutable.HashMap[Int, mutable.Stack[Symbol]], parseResult: ParseResult): ParseResult = {
      if (current.equals(0)) {
        ParseResult(parseResult.html, parseResult.css)
      }
      else {
        hashMap(current).length match {
          case a: Int if a == 0 && current > 1 =>
            factorialAcc(current - 1, hashMap, ParseResult(parseResult.html +printTab(current-1)+"</div>\n" , parseResult.css))
          case a: Int if a == 1 =>
            val a = hashMap(current).pop()
            a match {
              case layer: PackageSymbol =>
                layer.members.isEmpty match {
                  case true =>
                    factorialAcc(current - 1, hashMap, ParseResult(parseResult.html , parseResult.css))
                  case false =>
                    val s = mutable.Stack(layer.members: _*)
                    val nh = hashMap.+=((current + 1, s))
                    factorialAcc(current + 1, nh, ParseResult(parseResult.html+ printTab(current)+"<div>\n" , parseResult.css))
                }
              case layer: ImageSymbol =>
                val parent = layer.parentOpt.map(s => "." + s + " > ").getOrElse("")
                factorialAcc(current - 1, hashMap, ParseResult(parseResult.html + printTab(current)+ "<img class=\"" + layer.name.name + "\"" + "src=\""+ transformUrl(layer.imageUrl,framerConfig.projectId) + "\" />\n",
                  params2CssString(layer.params, parseResult.css + parent + " ." ++ layer.name.name + "{" + "\n") + "}\n"))
              case layer: TextSymbol =>
                val parent = layer.parentOpt.map(s => "." + s + " > ").getOrElse("")
                factorialAcc(current - 1, hashMap, ParseResult(parseResult.html +printTab(current)+ "<a class=\"" + layer.name.name + "\">" + layer.value + "</a>\n" +
                  printTab(current-1) +
                  "</div>\n",
                  params2CssString(layer.params, parseResult.css + parent + " ." ++ layer.name.name + "{" + "\n") + "}\n"))
              case layer: LayerSymbol =>
                val parent = layer.parentOpt.map(s => "." + s + " > ").getOrElse("")
                layer.members.isEmpty match {
                  case true =>
                    factorialAcc(current - 1, hashMap, ParseResult(parseResult.html +printTab(current)+ "<div class=\"" + layer.name.name + "\"></div>\n"+ printTab(current-1)+ "</div>\n",
                      params2CssString(layer.params, parseResult.css + parent + "." ++ layer.name.name + "{" + "\n") + "}\n"))
                  case false =>
                    val s = mutable.Stack(layer.members: _*)
                    val nh = hashMap.+=((current + 1, s))
                    factorialAcc(current + 1, nh, ParseResult(parseResult.html +printTab(current)+ "<div class=\"" + layer.name.name + "\">\n",
                      params2CssString(layer.params, parseResult.css + parent + "." ++ layer.name.name + "{" + "\n") + "}\n"))
                }
              case layer: PageSymbol =>
                layer.members.isEmpty match {
                  case true =>
                    factorialAcc(current - 1, hashMap, ParseResult(parseResult.html + printTab(current)+ "<div class=\"" + layer.name.name + "\"></div>\n",
                      params2CssString(layer.params, "." ++ layer.name.name + "{" + "\n") + "}\n"))
                  case false =>
                                  val s = mutable.Stack(layer.members: _*)
                                  val nh = hashMap.+=((current + 1, s))
                                  factorialAcc(current + 1, nh, ParseResult(parseResult.html +printTab(current)+ "<div class=\"" + layer.name.name + "\">\n",
                                    params2CssString(layer.params, "." ++ layer.name.name + "{" + "\n") + "}\n"))
                }
              case sy: Symbol =>
                println(sy);
                factorialAcc(current - 1, hashMap, ParseResult(parseResult.html +printTab(current)+ "<div class=\"" + sy.name.name + "\"></div>\n", parseResult.css))
            }
          case a: Int if a > 1 =>
            val layer = hashMap(current).pop()
            layer match {
              case layer: ImageSymbol =>
                val parent = layer.parentOpt.map(s => "." + s + " > ").getOrElse("")
                factorialAcc(current, hashMap, ParseResult(parseResult.html +printTab(current)+ "<img class=\"" + layer.name.name + "\"" + " src=\""+ transformUrl(layer.imageUrl,framerConfig.projectId) + "\" />\n",
                  params2CssString(layer.params, parseResult.css + parent + " ." ++ layer.name.name + "{" + "\n") + "}\n"))
              case layer: TextSymbol =>
                val parent = layer.parentOpt.map(s => "." + s + " > ").getOrElse("")
                factorialAcc(current, hashMap, ParseResult(parseResult.html +printTab(current)+ "<a class=\"" + layer.name.name + "\">" + layer.value + "</a>\n",
                  params2CssString(layer.params, parseResult.css + parent + " ." ++ layer.name.name + "{" + "\n") + "}\n"))
              case layer: LayerSymbol =>
                val parent = layer.parentOpt.map(s => "." + s + " > ").getOrElse("")
                layer.members.isEmpty match {
                  case true =>
                    factorialAcc(current, hashMap, ParseResult(parseResult.html +printTab(current)+ "<div class=\"" + layer.name.name + "\"></div>\n",
                      params2CssString(layer.params, parseResult.css + parent + "." ++ layer.name.name + "{" + "\n") + "}\n"))
                  case false =>
                    val s = mutable.Stack(layer.members: _*)
                    val nh = hashMap.+=((current + 1, s))
                    factorialAcc(current + 1, nh, ParseResult(parseResult.html +printTab(current)+ "<div class=\"" + layer.name.name + "\">\n",
                      params2CssString(layer.params, parseResult.css + parent + "." ++ layer.name.name + "{" + "\n") + "}\n"))
                }
              case layer: PageSymbol =>
                layer.members.isEmpty match {
                  case true =>
                    factorialAcc(current, hashMap, ParseResult(parseResult.html + printTab(current) + "<div class=\"" + layer.name.name + "\"></div>\n",
                      params2CssString(layer.params, "." ++ layer.name.name + "{" + "\n") + "}\n"))
                  case false =>
                    val s = mutable.Stack(layer.members: _*)
                    val nh = hashMap.+=((current + 1, s))
                    factorialAcc(current + 1, nh, ParseResult(parseResult.html + printTab(current) + "<div class=\"" + layer.name.name + "\">\n",
                      params2CssString(layer.params, "." ++ layer.name.name + "{" + "\n") + "}\n"))
                }
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

