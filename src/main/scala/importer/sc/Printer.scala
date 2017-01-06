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

  final def params2CssStringVnode(list1: List[TermTree], head1: String)(implicit framer: FramerConfig): String = {
    val head = "style: {" ;
    val ifResponsive = !framer.selectedHand.isEmpty;
    val SCREEN_WIDTH = framer.deviceType match {
      case "apple-iphone-5c-white" => 640
      case _ => 600
    }

    // width_ident
    val list2:List[TermTree] = list1.collectFirst {
      case str: WidthIdent => str
    } match {
      case Some(widthIdent) =>  list1
      case None => list1.+:(WidthIdent(StringIdent("0")))
    }

    val list3 = list2.collectFirst {
      case str: HeightIdent => str
    } match {
      case Some(widthIdent) => list2
      case None =>  list2.+:(HeightIdent(StringIdent("0")))
    }

    val list4 = list3.collectFirst {
      case str: XIdent => str
    } match {
      case Some(ident) => list3
      case None =>  list3.+:(XIdent(StringIdent("0")))
    }

    val list = list4.collectFirst {
      case str: YIdent => str
    } match {
      case Some(ident) => list4
      case None =>  list4.+:(YIdent(StringIdent("0")))
    }

    def hasBorderColor = list1.collectFirst {
      case border: BorderColorIdent => border
    }.isDefined

    val borderWithOpt:Option[Double] = list1.collectFirst {
      case BorderWidthIdent(v @ StringIdent(value)) => value.toDouble
    }

    val paddingOpt = list1.collectFirst {
      case PaddingIdent(v @StringIdent(value)) => value.toDouble
    }

    val isRelative:Boolean =
      list.collectFirst {
        case  XIdent(v @Value3Ident(pos,optCal,optStr)) if optCal == Some("pos") => v
        case  YIdent(v @Value3Ident(pos,optCal,optStr))if optCal == Some("pos") => v
      } match {
        case Some(ident) => true
        case None =>  false
      }

    val isText:Boolean = list.collectFirst {
      case  HtmlIdent(str) => str
    }.isDefined

    val paddingLeftWidth:Double = list.collectFirst  {
      case PaddingLeftIdent(v @StringIdent(value)) => value.toDouble
    } match {
      case Some(padding) => padding
      case None => paddingOpt  match {
        case Some(padding) => padding
        case None => 0
      }
    }

    val paddingRightWidth:Double = list.collectFirst  {
      case PaddingRightIdent(v @StringIdent(value)) => value.toDouble
    } match {
      case Some(d) => d
      case None => paddingOpt match {
        case Some(d) => d
        case None => 0
      }
    }

    val paddingTopWidth:Double = list.collectFirst  {
      case PaddingTopIdent(v @StringIdent(value)) => value.toDouble
    } match {
      case Some(padding) => padding
      case None => paddingOpt  match {
        case Some(padding) => padding
        case None => 0
      }
    }

    val paddingBottomWidth:Double = list.collectFirst  {
      case PaddingBottomIdent(v @StringIdent(value)) => value.toDouble
    } match {
      case Some(padding) => padding
      case None => paddingOpt  match {
        case Some(padding) => padding
        case None => 0
      }
    }


    def getPosition(): String = {
      if(!isRelative) "position: \"absolute\","
      else "position: \"relative,\" "
    }

    def getDisplay(): String = {
      if(!isText) "display: \"flex\", "
      else "display: \"inline-block\", "
    }

    def getBorderWidth(): String =
      list.collectFirst  {
        case BorderWidthIdent(v @StringIdent(value)) => value.toDouble
      } match {
        case Some(padding) => "";
        case None => "borderWidth: \"0px\", "
      }

    def textAlign(): String =
      list.collectFirst  {
        case StyleTextAlignIdent(v ) => v
      } match {
        case Some(v) => "textAlign: \""+v+"\",";
        case None => "textAlign: \"left\", "
      }

    def getPaddingWidth(): String =
      list.collectFirst  {
        case PaddingIdent(v @StringIdent(value)) => value.toDouble
        case PaddingLeftIdent(v @StringIdent(value)) => value.toDouble
        case PaddingTopIdent(v @StringIdent(value)) => value.toDouble
        case PaddingBottomIdent(v @StringIdent(value)) => value.toDouble
        case PaddingRightIdent(v @StringIdent(value)) => value.toDouble
      } match {
        case Some(padding) => "";
        case None => "padding: \"0px\", "
      }

    list.foldLeft(head + getDisplay + getPosition+getBorderWidth + getPaddingWidth+ textAlign)((result, term) =>
      term match {
        case HeightIdent(v@StringIdent(s1)) =>
          val s = s1.toDouble -paddingTopWidth - paddingBottomWidth - (2 * borderWithOpt.getOrElse(0.toDouble))
          if(ifResponsive) result + "height: " + s.toDouble / SCREEN_WIDTH  + "rem\","
          else result + "height: \"" + s + "px\", "
        case WidthIdent(v@ValueWithIdent(Ident("Screen"), value)) =>
          result + "width: \"100%\","
        case HeightIdent(v@ValueWithIdent(Ident("Screen"), value)) =>
          result + "height: \"100%\", "
        case WidthIdent(v@StringIdent(s1)) =>
          val s = s1.toDouble -paddingLeftWidth - paddingRightWidth- (2 * borderWithOpt.getOrElse(0.toDouble))
          if(ifResponsive) result + "width: \"" + s.toDouble / SCREEN_WIDTH  + "rem\"," else
            result + "width: \"" + s + "px\", "
        case ScrollVerticalIdent(BooleanValueIdent(b)) =>
          result
        case BorderWidthIdent(value@StringIdent(px)) =>
          val r = if(ifResponsive) result + "borderWidth: \"" + px.toDouble / SCREEN_WIDTH  + "rem\"," else
            result + "borderWidth: \"" + px + "px\", "
          if (r.contains("borderStyle")) r else r + "borderStyle: \"solid\", "
        case BackGroundColorIdent(color) =>
          result + "backgroundColor: \"" + color + "\","

        case YIdent(v @Value3Ident(pos,optCal,pxOpt)) =>
          pos match {
            case "bottom" if (optCal==Some("-")) =>
              if(ifResponsive) result + "bottom: \"" + pxOpt.get.toDouble / SCREEN_WIDTH  + "rem\"," else
                result + "bottom: \"" + pxOpt.get.toDouble + "px\"," + " "
            case "top" if (optCal==Some("-")) =>
              if(ifResponsive) result + "top: \"" + pxOpt.get.toDouble / SCREEN_WIDTH  + "rem\"," else
                result + "top: \"" + pxOpt.get.toDouble + "px\"," + " "
            case "center" => {
              if(result.contains("position"))
                result + "marginTop: \"auto\",marginBottom: \"auto\","
              else
                result + "marginTop: \"auto\"\nmarginBottom: auto;\n"
            }
          }
        case XIdent(v @Value3Ident(pos,optCal,pxOpt)) =>
          pos match {
            case "right" if (optCal==Some("-")) =>
              if(ifResponsive) result + "right: \"" + pxOpt.get.toDouble / SCREEN_WIDTH  + "rem\"," else
                result + "right: " + pxOpt.get.toDouble + "px;" + " "
            case "right" if (optCal==Some("+")) =>
              if(ifResponsive) result + "right: \"" + pxOpt.get.toDouble / SCREEN_WIDTH  + "rem\"," else
                result + "right: \"" + pxOpt.get.toDouble + "px\"," + " "
            case "center" => {
              if(result.contains("position"))
                result + "marginLeft: \"auto\", marginRight: \"auto\", "
              else
                result + "marginLeft: \"auto\", marginRight: \"auto\", "
            }
          }
        case XIdent(v@StringIdent(px)) =>
          if(ifResponsive) result + "left: \"" + px.toDouble / SCREEN_WIDTH  + "rem\"," else
            result + "left: \"" + px + "px\"," + " "
        case YIdent(v@StringIdent(px)) =>
          if(ifResponsive) result + "top: \"" + px.toDouble / SCREEN_WIDTH  + "rem\"," else
            result + "top: \"" + px + "px\"," + ""
        case VisibleIdent(isVisible) if isVisible == false =>
          result + "display: \"" + "hidden\" "
        case BorderRadiusIdent(v@StringIdent(value)) =>
          if(ifResponsive) result + "borderRadius: \"" + value.toDouble / SCREEN_WIDTH  + "rem\"," else
            result + "borderRadius: \"" + value + "px\", "
//        case StyleTextAlignIdent(v) =>
//          result + "textAlign: \"" + v + "\", "
        case LineHeightIdent(v) =>
          if(ifResponsive) result + "lineHeight: \"" + v.toDouble / SCREEN_WIDTH  + "rem\"," else
            result + "lineHeight: \"" + v + "px\", "
        case StyleFontSizeIdent(v) =>
          if(ifResponsive) result + "fontSize: \"" + v.toDouble / SCREEN_WIDTH  + "rem\"," else
            result + "fontSize: \"" + v + "px\", "
        case PaddingBottomIdent(v@StringIdent(value)) =>
          if(ifResponsive) result + "paddingBottom: \"" + value.toDouble / SCREEN_WIDTH  + "rem\"," else
            result + "paddingBottom: \"" + value +"px\", "
        case PaddingRightIdent(v@StringIdent(value)) =>
          if(ifResponsive) result + "paddingRight: \"" + value.toDouble / SCREEN_WIDTH  + "rem\"," else
            result + "padding-right: \"" + value +"px\", "
        case MarginTopIdent(v@StringIdent(value)) =>
          if(ifResponsive) result + "marginTop: \"" + value.toDouble / SCREEN_WIDTH  + "rem\"," else
            result + "marginTop: \"" + value +"px\", "
        case MarginRightIdent(v@StringIdent(value)) =>
          if(ifResponsive) result + "marginRight: \"" + value.toDouble / SCREEN_WIDTH  + "rem\"," else
            result + "marginRight: \"" + value +"px\", "
        case MarginLeftIdent(v@StringIdent(value)) =>
          if(ifResponsive) result + "marginLeft: \"" + value.toDouble / SCREEN_WIDTH  + "rem\"," else
            result + "marginLeft: \"" + value +"px\", "
        case MarginBottomIdent(v@StringIdent(value)) =>
          if(ifResponsive) result + "marginBottom: \"" + value.toDouble / SCREEN_WIDTH  + "rem\"," else
            result + "marginBottom: \"" + value +"px\", "
        case PaddingTopIdent(v@StringIdent(value)) =>
          if(ifResponsive) result + "paddingTop: \"" + value.toDouble / SCREEN_WIDTH  + "rem\"," else
            result + "paddingTop: \"" + value +"px\", "
        case PaddingRightIdent(v@StringIdent(value)) =>
          if(ifResponsive) result + "paddingRight: " + value.toDouble / SCREEN_WIDTH  + "rem\"," else
            result + "paddingRight: \"" + value +"px\", "
        case PaddingLeftIdent(v@StringIdent(value)) =>
          if(ifResponsive) result + "paddingLeft: \"" + value.toDouble / SCREEN_WIDTH  + "rem;\"," else
            result + "paddingLeft: \"" + value +"px\", "
        case BorderColorIdent(v) =>
          if(ifResponsive) result + "borderColor: \"" + v.toDouble / SCREEN_WIDTH  + "\"," else
            result + "borderColor: \"" + v +"\", "
        case FontColorIdent(v) =>
          result + "color: \"" + v + "\","
        case OpacityIdent(v) =>
          result + "opacity: \"" + v + "\", "
        case _ =>
          result;
      }) +"}"
  }
  final def params2CssString(list1: List[TermTree], head: String)(implicit framer: FramerConfig): String = {
    val ifResponsive = !framer.selectedHand.isEmpty;
    val SCREEN_WIDTH = framer.deviceType match {
      case "apple-iphone-5c-white" => 640
      case _ => 600
    }

    // width_ident
    val list2:List[TermTree] = list1.collectFirst {
      case str: WidthIdent => str
    } match {
      case Some(widthIdent) =>  list1
      case None => list1.+:(WidthIdent(StringIdent("0")))
    }

    val list3 = list2.collectFirst {
      case str: HeightIdent => str
    } match {
      case Some(widthIdent) => list2
      case None =>  list2.+:(HeightIdent(StringIdent("0")))
    }

    val list4 = list3.collectFirst {
      case str: XIdent => str
    } match {
      case Some(ident) => list3
      case None =>  list3.+:(XIdent(StringIdent("0")))
    }

    val list = list4.collectFirst {
      case str: YIdent => str
    } match {
      case Some(ident) => list4
      case None =>  list4.+:(YIdent(StringIdent("0")))
    }

    def hasBorderColor = list1.collectFirst {
      case border: BorderColorIdent => border
    }.isDefined

    val borderWithOpt:Option[Double] = list1.collectFirst {
      case BorderWidthIdent(v @ StringIdent(value)) => value.toDouble
    }

    val paddingOpt = list1.collectFirst {
      case PaddingIdent(v @StringIdent(value)) => value.toDouble
    }

    val isRelative:Boolean =
          list.collectFirst {
      case  XIdent(v @Value3Ident(pos,optCal,optStr)) if optCal == Some("pos") => v
      case  YIdent(v @Value3Ident(pos,optCal,optStr))if optCal == Some("pos") => v
    } match {
      case Some(ident) => true
      case None =>  false
    }

    val isText:Boolean = list.collectFirst {
      case  HtmlIdent(str) => str
    }.isDefined

    val paddingLeftWidth:Double = list.collectFirst  {
      case PaddingLeftIdent(v @StringIdent(value)) => value.toDouble
    } match {
      case Some(padding) => padding
      case None => paddingOpt  match {
        case Some(padding) => padding
        case None => 0
      }
    }

    val paddingRightWidth:Double = list.collectFirst  {
      case PaddingRightIdent(v @StringIdent(value)) => value.toDouble
    } match {
      case Some(d) => d
      case None => paddingOpt match {
        case Some(d) => d
        case None => 0
      }
    }

    val paddingTopWidth:Double = list.collectFirst  {
      case PaddingTopIdent(v @StringIdent(value)) => value.toDouble
    } match {
      case Some(padding) => padding
      case None => paddingOpt  match {
        case Some(padding) => padding
        case None => 0
      }
    }

    val paddingBottomWidth:Double = list.collectFirst  {
      case PaddingBottomIdent(v @StringIdent(value)) => value.toDouble
    } match {
      case Some(padding) => padding
      case None => paddingOpt  match {
        case Some(padding) => padding
        case None => 0
      }
    }



    def getPosition(): String = {
      if(!isRelative) "position: absolute;\n"
      else "position: relative;\n"
    }

    def getDisplay(): String = {
      if(!isText) "display: flex;\n"
      else "display: inline-block;\n"
    }

    def getBorderWidth(): String =
      list.collectFirst  {
        case BorderWidthIdent(v @StringIdent(value)) => value.toDouble
      } match {
        case Some(padding) => "";
        case None => "border-width: 0px;\n"
      }

    def getPaddingWidth(): String =
      list.collectFirst  {
        case PaddingIdent(v @StringIdent(value)) => value.toDouble
        case PaddingLeftIdent(v @StringIdent(value)) => value.toDouble
        case PaddingTopIdent(v @StringIdent(value)) => value.toDouble
        case PaddingBottomIdent(v @StringIdent(value)) => value.toDouble
        case PaddingRightIdent(v @StringIdent(value)) => value.toDouble
      } match {
        case Some(padding) => "";
        case None => "padding: 0px;\n"
      }

    def textAlign(): String =
      list.collectFirst  {
        case StyleTextAlignIdent(v ) => v
      } match {
        case Some(v) => "text-align: "+v+";\n";
        case None => "text-align: left;\n";
      }

   list.foldLeft(head + getDisplay + getPosition+getBorderWidth + getPaddingWidth+ textAlign)((result, term) =>
      term match {
        case HeightIdent(v@StringIdent(s1)) =>
          val s = s1.toDouble -paddingTopWidth - paddingBottomWidth - (2 * borderWithOpt.getOrElse(0.toDouble))
          if(ifResponsive) result + "height: " + s.toDouble / SCREEN_WIDTH  + "rem;\n"
          else result + "height: " + s + "px;\n"
        case WidthIdent(v@ValueWithIdent(Ident("Screen"), value)) =>
          result + "width: 100%;\n"
        case HeightIdent(v@ValueWithIdent(Ident("Screen"), value)) =>
          result + "height: 100%;\n"
        case WidthIdent(v@StringIdent(s1)) =>
          val s = s1.toDouble -paddingLeftWidth - paddingRightWidth- (2 * borderWithOpt.getOrElse(0.toDouble))
          if(ifResponsive) result + "width: " + s.toDouble / SCREEN_WIDTH  + "rem;\n" else
          result + "width: " + s + "px;\n"
        case ScrollVerticalIdent(BooleanValueIdent(b)) =>
          result
        case BorderWidthIdent(value@StringIdent(px)) =>
          val r = if(ifResponsive) result + "border-width: " + px.toDouble / SCREEN_WIDTH  + "rem;\n" else
          result + "border-width: " + px + "px;\n"
          if (r.contains("border-style")) r else r + "border-style: solid;\n"
        case BackGroundColorIdent(color) =>
          result + "background-color: " + color + ";\n"

        case YIdent(v @Value3Ident(pos,optCal,pxOpt)) =>
          pos match {
            case "bottom" if (optCal==Some("-")) =>
              if(ifResponsive) result + "bottom: " + pxOpt.get.toDouble / SCREEN_WIDTH  + "rem;\n" else
                result + "bottom: " + pxOpt.get.toDouble + "px;" + "\n"
            case "top" if (optCal==Some("-")) =>
              if(ifResponsive) result + "top: " + pxOpt.get.toDouble / SCREEN_WIDTH  + "rem;\n" else
                result + "top: " + pxOpt.get.toDouble + "px;" + "\n"
            case "center" => {
              if(result.contains("position"))
              result + "margin-top: auto;\nmargin-bottom: auto;\n"
              else
                result + "margin-top: auto;\nmargin-bottom: auto;\n"
            }
          }
        case XIdent(v @Value3Ident(pos,optCal,pxOpt)) =>
          pos match {
            case "right" if (optCal==Some("-")) =>
              if(ifResponsive) result + "right: " + pxOpt.get.toDouble / SCREEN_WIDTH  + "rem;\n" else
                result + "right: " + pxOpt.get.toDouble + "px;" + "\n"
            case "right" if (optCal==Some("+")) =>
              if(ifResponsive) result + "right: " + pxOpt.get.toDouble / SCREEN_WIDTH  + "rem;\n" else
                result + "right: -" + pxOpt.get.toDouble + "px;" + "\n"
            case "center" => {
              if(result.contains("position"))
               result + "margin-left: auto;\nmargin-right: auto;\n"
              else
                result + "margin-left: auto;\nmargin-right: auto;\n"
            }
          }
        case XIdent(v@StringIdent(px)) =>
          if(ifResponsive) result + "left: " + px.toDouble / SCREEN_WIDTH  + "rem;\n" else
            result + "left: " + px + "px;" + "\n"
        case YIdent(v@StringIdent(px)) =>
          if(ifResponsive) result + "top: " + px.toDouble / SCREEN_WIDTH  + "rem;\n" else
          result + "top: " + px + "px;" + "\n"
        case VisibleIdent(isVisible) if isVisible == false =>
          result + "display: " + "hidden;\n"
        case BorderRadiusIdent(v@StringIdent(value)) =>
          if(ifResponsive) result + "border-radius: " + value.toDouble / SCREEN_WIDTH  + "rem;\n" else
          result + "border-radius: " + value + "px;\n"
//        case StyleTextAlignIdent(v) =>
//          result + "text-align: " + v + ";\n"
        case LineHeightIdent(v) =>
          if(ifResponsive) result + "line-height: " + v.toDouble / SCREEN_WIDTH  + "rem;\n" else
          result + "line-height: " + v + "px;\n"
        case StyleFontSizeIdent(v) =>
          if(ifResponsive) result + "font-size: " + v.toDouble / SCREEN_WIDTH  + "rem;\n" else
          result + "font-size: " + v + "px;\n"
        case MarginBottomIdent(v@StringIdent(value)) =>
          if(ifResponsive) result + "margin-bottom: " + value.toDouble / SCREEN_WIDTH  + "rem;\n" else
            result + "margin-bottom: " + value +"px;\n"
        case MarginTopIdent(v@StringIdent(value)) =>
          if(ifResponsive) result + "margin-top: " + value.toDouble / SCREEN_WIDTH  + "rem;\n" else
            result + "margin-top: " + value +"px;\n"
        case MarginLeftIdent(v@StringIdent(value)) =>
          if(ifResponsive) result + "margin-left: " + value.toDouble / SCREEN_WIDTH  + "rem;\n" else
            result + "margin-left: " + value +"px;\n"
        case MarginRightIdent(v@StringIdent(value)) =>
          if(ifResponsive) result + "margin-right: " + value.toDouble / SCREEN_WIDTH  + "rem;\n" else
            result + "margin-right: " + value +"px;\n"
        case PaddingBottomIdent(v@StringIdent(value)) =>
          if(ifResponsive) result + "padding-bottom: " + value.toDouble / SCREEN_WIDTH  + "rem;\n" else
            result + "padding-bottom: " + value +"px;\n"
        case PaddingRightIdent(v@StringIdent(value)) =>
          if(ifResponsive) result + "padding-right: " + value.toDouble / SCREEN_WIDTH  + "rem;\n" else
            result + "padding-right: " + value +"px;\n"
        case PaddingTopIdent(v@StringIdent(value)) =>
          if(ifResponsive) result + "padding-top: " + value.toDouble / SCREEN_WIDTH  + "rem;\n" else
            result + "padding-top: " + value +"px;\n"
        case PaddingRightIdent(v@StringIdent(value)) =>
          if(ifResponsive) result + "padding-right: " + value.toDouble / SCREEN_WIDTH  + "rem;\n" else
            result + "padding-right: " + value +"px;\n"
        case PaddingLeftIdent(v@StringIdent(value)) =>
          if(ifResponsive) result + "padding-left: " + value.toDouble / SCREEN_WIDTH  + "rem;\n" else
            result + "padding-left: " + value +"px;\n"
        case BorderColorIdent(v) =>
          if(ifResponsive) result + "border-color: " + v.toDouble / SCREEN_WIDTH  + ";\n" else
            result + "border-color: " + v +";\n"
        case FontColorIdent(v) =>
          result + "color: " + v + ";\n"
        case OpacityIdent(v) =>
          result + "opacity: " + v + ";\n"
        case _ =>
          result;
      })
  }

  final def printSymbolVNode(initialSym: Symbol, framerConfig: FramerConfig): ParseResult = {
    implicit val framer:FramerConfig = framerConfig;
    @tailrec def factorialAcc(current: Int, hashMap: mutable.HashMap[Int, mutable.Stack[Symbol]], parseResult: ParseResult): ParseResult = {
      if (current.equals(0)) {
        ParseResult(parseResult.html, parseResult.css)
      }
      else {
        hashMap(current).length match {
          case a: Int if a == 0 && current > 0 =>
            factorialAcc(current - 1, hashMap, ParseResult(parseResult.html +printTab(current)+"])" , parseResult.css))
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
                    factorialAcc(current + 1, nh, ParseResult(parseResult.html+
                      printTab(current)+"div(\"\",{},["+"\n" , parseResult.css))
                }
              case layer: ImageSymbol =>
                val parent = layer.parentOpt.map(s => "." + s + " > ").getOrElse("")
//                factorialAcc(current - 1, hashMap, ParseResult(parseResult.html + printTab(current)+ "<img class=\"" + layer.name.name + "\"" + "src=\""+ transformUrl(layer.imageUrl,framerConfig.projectId) + "\" />\n",
//                  params2CssString(layer.params, parseResult.css + parent + " ." ++ layer.name.name + "{" + "\n") + "}\n"))
                factorialAcc(current - 1, hashMap, ParseResult(parseResult.html +printTab(current)+ "img(\"."+layer.name.name+"\",{"+" props: { src: \""+layer.imageUrl+"\"},"+params2CssStringVnode(layer.params,"")+"})\n",
                  params2CssString(layer.params, parseResult.css + parent + " ." ++ layer.name.name + "{" + "\n") + "}\n"))
              case layer: InputSymbol =>
                val parent = layer.parentOpt.map(s => "." + s + " > ").getOrElse("")
                factorialAcc(current - 1, hashMap, ParseResult(parseResult.html +printTab(current)+ "input(\"."+layer.name.name+"\",{"+" props: { type: \""+layer.inputType+"\", placeHolder: \""+layer.value+"\",},"+params2CssStringVnode(layer.params,"")+"})\n",
                  params2CssString(layer.params, parseResult.css + " ." + parent + layer.name.name + "{" + "\n") + "}\n"))
//                factorialAcc(current - 1, hashMap, ParseResult(parseResult.html +printTab(current)+ "<input type=\""+layer.inputType + "\" placeHolder=\""+layer.value + "\"  class=\"" + layer.name.name + "\" />\n" +
//                  printTab(current-1) +
//                  "</div>\n",
//                  params2CssString(layer.params, parseResult.css + parent + " ." ++ layer.name.name + "{" + "\n") + "}\n"))
              case layer: TextSymbol =>
                val parent = layer.parentOpt.map(s => "." + s + " > ").getOrElse("")
                factorialAcc(current - 1, hashMap, ParseResult(parseResult.html +printTab(current)+ "a(.\""+layer.name.name+"\",{"+params2CssStringVnode(layer.params,"")+"}, \""+layer.value+"\")\n",
                  params2CssString(layer.params, parseResult.css + parent + " ." ++ layer.name.name + "{" + "\n") + "}\n"))
              case layer: LayerSymbol =>
                val parent = layer.parentOpt.map(s => "." + s + " > ").getOrElse("")
                layer.members.isEmpty match {
                  case true =>
                    factorialAcc(current - 1, hashMap, ParseResult(parseResult.html +
                      printTab(current) +"div(\"."+layer.name.name+"\",{"+params2CssStringVnode(layer.params,"")+"},[])\n",
//                       "<div class=\"" + layer.name.name + "\"></div>\n"+ printTab(current-1)+ "</div>\n",
                      params2CssString(layer.params, parseResult.css + parent + "." ++ layer.name.name + "{" + "\n") + "}\n"))
                  case false =>
                    val s = mutable.Stack(layer.members: _*)
                    val nh = hashMap.+=((current + 1, s))
                    factorialAcc(current + 1, nh, ParseResult(parseResult.html +printTab(current)+ "<div class=\"" + layer.name.name + "\">\n",
                      params2CssString(layer.params, parseResult.css + parent + "." ++ layer.name.name + "{" + "\n") + "}\n"))
                }
//              case layer: PageSymbol =>
//                layer.members.isEmpty match {
//                  case true =>
//                    factorialAcc(current - 1, hashMap, ParseResult(parseResult.html + printTab(current)+ "<div class=\"" + layer.name.name + "\"></div>\n",
//                      params2CssString(layer.params, "." ++ layer.name.name + "{" + "\n") + "}\n"))
//                  case false =>
//                    val s = mutable.Stack(layer.members: _*)
//                    val nh = hashMap.+=((current + 1, s))
//                    factorialAcc(current + 1, nh, ParseResult(parseResult.html +printTab(current)+ "<div class=\"" + layer.name.name + "\">\n",
//                      params2CssString(layer.params, "." ++ layer.name.name + "{" + "\n") + "}\n"))
//                }
              case sy: Symbol =>
                factorialAcc(current - 1, hashMap, ParseResult(parseResult.html +printTab(current)+ "<div class=\"" + sy.name.name + "\"></div>\n", parseResult.css))
            }
          case a: Int if a > 1 =>
            val layer = hashMap(current).pop()
            layer match {
//              case layer: ImageSymbol =>
//                val parent = layer.parentOpt.map(s => "." + s + " > ").getOrElse("")
//                factorialAcc(current, hashMap, ParseResult(parseResult.html +printTab(current)+ "<img class=\"" + layer.name.name + "\"" + " src=\""+ transformUrl(layer.imageUrl,framerConfig.projectId) + "\" />\n",
//                  params2CssString(layer.params, parseResult.css + parent + " ." ++ layer.name.name + "{" + "\n") + "}\n"))
//              case layer: InputSymbol =>
//                val parent = layer.parentOpt.map(s => "." + s + " > ").getOrElse("")
//                factorialAcc(current, hashMap, ParseResult(parseResult.html +printTab(current)+ "<input type=\""+ layer.inputType+"\" placeHolder=\""+layer.value + "\" class=\"" + layer.name.name + "\" />\n",
//                  params2CssString(layer.params, parseResult.css + parent + " ." ++ layer.name.name + "{" + "\n") + "}\n"))
//              case layer: TextSymbol =>
//                val parent = layer.parentOpt.map(s => "." + s + " > ").getOrElse("")
//                factorialAcc(current, hashMap, ParseResult(parseResult.html +printTab(current)+ "<a class=\"" + layer.name.name + "\">" + layer.value + "</a>\n",
//                  params2CssString(layer.params, parseResult.css + parent + " ." ++ layer.name.name + "{" + "\n") + "}\n"))
//              case layer: LayerSymbol =>
//                val parent = layer.parentOpt.map(s => "." + s + " > ").getOrElse("")
//                layer.members.isEmpty match {
//                  case true =>
//                    factorialAcc(current, hashMap, ParseResult(parseResult.html +printTab(current)+ "<div class=\"" + layer.name.name + "\"></div>\n",
//                      params2CssString(layer.params, parseResult.css + parent + "." ++ layer.name.name + "{" + "\n") + "}\n"))
//                  case false =>
//                    val s = mutable.Stack(layer.members: _*)
//                    val nh = hashMap.+=((current + 1, s))
//                    factorialAcc(current + 1, nh, ParseResult(parseResult.html +printTab(current)+ "<div class=\"" + layer.name.name + "\">\n",
//                      params2CssString(layer.params, parseResult.css + parent + "." ++ layer.name.name + "{" + "\n") + "}\n"))
//                }
//              case layer: PageSymbol =>
//                layer.members.isEmpty match {
//                  case true =>
//                    factorialAcc(current, hashMap, ParseResult(parseResult.html + printTab(current) + "<div class=\"" + layer.name.name + "\"></div>\n",
//                      params2CssString(layer.params, "." ++ layer.name.name + "{" + "\n") + "}\n"))
//                  case false =>
//                    val s = mutable.Stack(layer.members: _*)
//                    val nh = hashMap.+=((current + 1, s))
//                    factorialAcc(current + 1, nh, ParseResult(parseResult.html + printTab(current) + "<div class=\"" + layer.name.name + "\">\n",
//                      params2CssString(layer.params, "." ++ layer.name.name + "{" + "\n") + "}\n"))
//                }
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
    factorialAcc(1, hm, ParseResult("", ""))
  }

  final def printSymbol(initialSym: Symbol, framerConfig: FramerConfig): ParseResult = {
    implicit val framer:FramerConfig = framerConfig;
    @tailrec def factorialAcc(current: Int, hashMap: mutable.HashMap[Int, mutable.Stack[Symbol]], parseResult: ParseResult): ParseResult = {
      if (current.equals(0)) {
        ParseResult(parseResult.html, parseResult.css)
      }
      else {
        hashMap(current).length match {
          case a: Int if a == 0 && current > 0 =>
            factorialAcc(current - 1, hashMap, ParseResult(parseResult.html +printTab(current)+"</div>\n" , parseResult.css))
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
              case layer: InputSymbol =>
                val parent = layer.parentOpt.map(s => "." + s + " > ").getOrElse("")
                factorialAcc(current - 1, hashMap, ParseResult(parseResult.html +printTab(current)+ "<input type=\""+layer.inputType + "\" placeHolder=\""+layer.value + "\"  class=\"" + layer.name.name + "\" />\n" +
                  printTab(current-1) +
                  "</div>\n",
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
                factorialAcc(current - 1, hashMap, ParseResult(parseResult.html +printTab(current)+ "<div class=\"" + sy.name.name + "\"></div>\n", parseResult.css))
            }
          case a: Int if a > 1 =>
            val layer = hashMap(current).pop()
            layer match {
              case layer: ImageSymbol =>
                val parent = layer.parentOpt.map(s => "." + s + " > ").getOrElse("")
                factorialAcc(current, hashMap, ParseResult(parseResult.html +printTab(current)+ "<img class=\"" + layer.name.name + "\"" + " src=\""+ transformUrl(layer.imageUrl,framerConfig.projectId) + "\" />\n",
                  params2CssString(layer.params, parseResult.css + parent + " ." ++ layer.name.name + "{" + "\n") + "}\n"))
              case layer: InputSymbol =>
                val parent = layer.parentOpt.map(s => "." + s + " > ").getOrElse("")
                factorialAcc(current, hashMap, ParseResult(parseResult.html +printTab(current)+ "<input type=\""+ layer.inputType+"\" placeHolder=\""+layer.value + "\" class=\"" + layer.name.name + "\" />\n",
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

