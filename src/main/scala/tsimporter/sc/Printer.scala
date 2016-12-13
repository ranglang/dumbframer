/* TypeScript importer for Scala.js
 * Copyright 2013-2014 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package tsimporter.sc

import java.io.PrintWriter

import tsimporter.Trees._

import scala.annotation.tailrec
import scala.collection.mutable

class Printer(
              outputPackage: String) {
  private implicit val self = this
  private var currentJSNamespace = ""


  //  def printTermPara(paras: List[TermTree]): Unit = {
  //    for (para <- paras) {
  //      para match {
  //        case BorderRadiusIdent (vale) =>
  //        case PointIdent(str) =>
  //        case BorderWidthIdent(a @ ValueIdent(b)) =>
  //          plncss"border-width:${b}"
  //        case VisibleIdent (ident) =>
  //          plncss"visible:${ident.name}"
  //
  //        case SizeIdent(ident) =>
  //        case image: ImageIdent =>
  //          val a = "\""+image.value +"\""
  //          plncss"backgroundImage:$a"
  //        case s: StyleIdent =>
  //          plncss"${s.paraName}:${s.paraValue}"
  //        case v: HeightIdent =>
  //          plncss"height:${v.value}"
  //        case w: WidthIdent =>
  //          plncss"width:${w.value}"
  //        case YIdent(alignDate) => alignDate match {
  //          case Value3Ident(position,opts,Some(num)) =>
  //            opts match {
  //              case  Some(str) => position match {
  //                case "right" =>
  //                  plncss"y:0"
  //                case "left" =>
  //                  plncss"x:0"
  //                case "center" =>
  //                  plncss"margin-left:auto"
  //                  plncss"margin-right:auto"
  //                case a: String =>
  //                  plncss"margin-left:${a}"
  //
  //              }
  //              case None => position match {
  //                case "right" =>
  //                  plncss"y:0"
  //                case "left" =>
  //                  plncss"x:0"
  //                case "center" =>
  //                  plncss"margin-left:auto"
  //                  plncss"margin-right:auto"
  //                case a: String =>
  //                  plncss"margin-left:${a}"
  //              }
  //            }
  //        }
  //        case XIdent(alignDate) => alignDate match {
  //          case Value3Ident(position,opts,Some(num)) =>
  //            opts match {
  //              case  Some(str) => position  match {
  //                case "right" =>
  //                  plncss"y:0"
  //                case "left" =>
  //                  plncss"x:0"
  //                case "center" =>
  //                  plncss"margin-left:auto"
  //                  plncss"margin-right:auto"
  //                case a: String =>
  //                  plncss"margin-left:${a}"
  //
  //              }
  //              case None => position match {
  //                case "right" =>
  //                  plncss"y:0"
  //                case "left" =>
  //                  plncss"x:0"
  //                case "center" =>
  //                  plncss"margin-left:auto"
  //                  plncss"margin-right:auto"
  //                case a: String =>
  //                  plncss"margin-left:${a}"
  //              }
  //            }
  //        }
  //        case b: BackGroundColorIdent =>
  //          plncss"background-color:${b.value}"
  //        case _ => Console.println("..")
  //      }
  //    }
  //
  //  }

  final def printSymbol(syms: Symbol, head: String): String = {
    var s :Set[Symbol]= Set()
    @tailrec def factorialAcc(slice: mutable.Stack[Symbol], head: String): String = {
      if(slice.isEmpty) head else
        {
          val sym = slice.head
          sym match {
            case sym: PackageSymbol =>
              slice.pop();
              if (!sym.members.isEmpty) {
                val (topLevels, packageObjectMembers) =
                  sym.members.partition(canBeTopLevel)
                factorialAcc(slice.pushAll(sym.members), head)
              } else
                head
            case PageSymbol(name, params, list) =>
              slice.pop();
              head
            case layer: LayerSymbol =>
              if(s(layer)){
                factorialAcc(slice.drop(1), head + layer.name.name)
              } else {
                s = s.+(layer)
                if(!layer.members.isEmpty) {
                  factorialAcc(slice.pushAll(layer.members), head)
                } else {
                  factorialAcc(slice.drop(1), head + layer.name.name)
                }
              }
            case comment: CommentSymbol =>
              slice.pop();
              factorialAcc(slice, head + comment.name.name)
            case layer: TextSymbol =>
              slice.pop()
              factorialAcc(slice, head + layer.name.name)
            case a: Symbol =>
              head
          }
        }
    }
    factorialAcc(mutable.Stack(syms), head)
  }

  private def canBeTopLevel(sym: Symbol): Boolean =
    sym.isInstanceOf[ContainerSymbol]
}

