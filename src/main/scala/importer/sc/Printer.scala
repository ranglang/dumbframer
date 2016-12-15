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

    val hashMap: mutable.HashMap[Int, mutable.Stack[Symbol]] = new mutable.HashMap[Int, mutable.Stack[Symbol]]()
    var s: Set[Symbol] = Set()
    @tailrec def factorialAcc(current: Int, hashMap: mutable.HashMap[Int, mutable.Stack[Symbol]], head: String): String = {

      println("factorialAcc ")
      if (current.equals(0)) {
        // && hashMap.get(0).size.equals(0)){
        println("return ")
        head
      }
      else {
        println("continute ")
        hashMap(current).length match {
          case a: Int if a == 0 =>
            println("hashMap.length == 0")
            factorialAcc(current - 1, hashMap, head + "</div>")
          case a: Int if a == 1 =>
            println("hashMap.length == 1")
            println("hashMap(current).length): " + hashMap(current).length)
            val a = hashMap(current).pop()
            println("hashMap(current).length): " + hashMap(current).length)
            a match {
              case layer: PackageSymbol =>
                println("package members" + layer.members)
                layer.members.isEmpty match {
                  case true =>
                    println("is Empty")
                    factorialAcc(current - 1, hashMap, head + "<div>" + layer.name.name + "</div>" + "</div>")
                  case false =>
                    println("Not Empty")
                    val s = mutable.Stack(layer.members: _*)
                    val nh = hashMap.+=((current + 1, s))
                    factorialAcc(current + 1, nh, head + "<div>" + layer.name.name)
                }
              case layer: LayerSymbol =>
                layer.members.isEmpty match {
                  case true =>
                    println("is Empty")
                    factorialAcc(current - 1, hashMap, head + "<div>" + layer.name.name + "</div>" + "</div>")
                  case false =>
                    println("Not Empty")
                    val s = mutable.Stack(layer.members: _*)
                    val nh = hashMap.+=((current + 1, s))
                    factorialAcc(current + 1, nh, head + "<div>" + layer.name.name)
                }
              case sy: Symbol =>
                println("otherSymbole")
                println("other: " + sy)
                factorialAcc(current - 1, hashMap, head + "<div>" + sy.name.name + "</div>" + "</div>")
            }
          case a: Int if a > 1 =>
            println("hashMap.length > 1")
            val a = hashMap(current).pop()
            factorialAcc(current, hashMap, head + "<div>" + a.name.name + "</div>" + "</div>")
          case _ =>
            println("else")
            ""
        }
      }
    }

    val initailStack: mutable.Stack[Symbol] = mutable.Stack(initialSym)
    val hm: mutable.HashMap[Int, mutable.Stack[Symbol]] = new mutable.HashMap[Int, mutable.Stack[Symbol]]()
    hm.put(1, initailStack)
    println("hm.size: " + hm.size);
    println(hm.size);
    factorialAcc(1, hm, head)
  }
}

