/* TypeScript importer for Scala.js
 * Copyright 2013-2014 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package importer.sc

import scala.annotation.tailrec
import scala.collection.mutable

object Printer {
  private implicit val self = this

  final def printSymbol(initialSym: Symbol, head: String): String = {

    val hashMap: mutable.HashMap[Int,mutable.Stack[Symbol]] = new mutable.HashMap[Int,mutable.Stack[Symbol]]()
    var s: Set[Symbol] = Set()
    @tailrec def factorialAcc(current: Int, hashMap: mutable.HashMap[Int,mutable.Stack[Symbol]], head: String): String = {
      if(current.equals(1) && hashMap.get(current).size.equals(0)) head
      else {
        hashMap(current).length match {
          case 0 =>
            factorialAcc(current-1,hashMap,head+"/div")
          case 1 =>
            val a  = hashMap(current).pop()
            a match {
              case layer: LayerSymbol =>
                layer.members.isEmpty match {
                  case true =>
                    factorialAcc(current-1,hashMap,head+"<div>"+layer.name+"</>"+"/div")
                  case false =>
                    val  s =  mutable.Stack(layer.members:_*)
                    val nh= hashMap.+=((1,s))
                    factorialAcc(current+1,nh,head+"<div>"+layer.name)
                }
              case sy:Symbol =>
                factorialAcc(current-1,hashMap,head+"<div>"+sy.name+"</>"+"/div")
            }
          case _ =>
            val a  = hashMap(current).pop()
            factorialAcc(current,hashMap,head+"<div>"+a.name+"</>"+"/div")
        }
      }
//      if (slice.isEmpty) head
//      else {
//        val sym = slice.head
//        sym match {
//          case sym: PackageSymbol =>
//            slice.pop();
//            if (!sym.members.isEmpty) {
//              factorialAcc(slice.pushAll(sym.members), head)
//            } else
//              head
//          case PageSymbol(name, params, list) =>
//            slice.pop();
//            head
//          case layer: LayerSymbol =>
//            if (s(layer)) {
//              val content = "<div>"+layer.name.name + "</div>"
//              factorialAcc(slice.drop(1), head + content)
//            } else {
//              s = s.+(layer)
//              if (!layer.members.isEmpty) {
//                factorialAcc(slice.pushAll(layer.members),head)
//              } else {
//                val content = "<div>"+layer.name.name + "</div>"
//                factorialAcc(slice.drop(1), head + content)
//              }
//            }
//          case comment: CommentSymbol =>
//            slice.pop();
//            val s = "<!-->"+comment.name.name + "</-->"
//            factorialAcc(slice, head + s)
//          case layer: TextSymbol =>
//            slice.pop()
//            factorialAcc(slice, head + layer.name.name)
//          case layer: NotSupportSymbol =>
//            layer.value
//          case a: Symbol =>
//            println(a);
//            head
//        }
//      }
    }

    val initailStack:mutable.Stack[Symbol] =  mutable.Stack(initialSym)
    val hm: mutable.HashMap[Int,mutable.Stack[Symbol]] = new mutable.HashMap[Int,mutable.Stack[Symbol]]()
    hm.put(0,initailStack)
     factorialAcc(0,hm, head)
  }
}

