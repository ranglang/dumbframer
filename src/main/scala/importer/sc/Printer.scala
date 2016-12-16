/* TypeScript importer for Scala.js
 * Copyright 2013-2014 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package importer.sc

import importer.ParseResult

import scala.annotation.tailrec
import scala.collection.mutable

object Printer {
  private implicit val self = this

  final def printSymbol(initialSym: Symbol, head: String): ParseResult = {

    val hashMap: mutable.HashMap[Int, mutable.Stack[Symbol]] = new mutable.HashMap[Int, mutable.Stack[Symbol]]()
    var s: Set[Symbol] = Set()
    @tailrec def factorialAcc(current: Int, hashMap: mutable.HashMap[Int, mutable.Stack[Symbol]], parseResult: ParseResult): ParseResult = {

      println("factorialAcc ")
      if (current.equals(0)) {
        println("return ")
        parseResult
      }
      else {
        println("continute ")
        hashMap(current).length match {
          case a: Int if a == 0 =>
            println("hashMap.length == 0")
            factorialAcc(current - 1, hashMap, ParseResult (parseResult.html + "</div>",parseResult.css))
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
                    factorialAcc(current - 1, hashMap, ParseResult(parseResult.html + "<div>" + layer.name.name + "</div>" + "</div>", parseResult.css))
                  case false =>
                    println("Not Empty")
                    val s = mutable.Stack(layer.members: _*)
                    val nh = hashMap.+=((current + 1, s))
                    factorialAcc(current + 1, nh, ParseResult(parseResult.html + "<div>" + layer.name.name,parseResult.css+layer.name.name))
                }
              case layer: LayerSymbol =>
                layer.members.isEmpty match {
                  case true =>
                    println("is Empty")
                    factorialAcc(current - 1, hashMap, ParseResult(parseResult.html + "<div>" + layer.name.name + "</div>" + "</div>",parseResult.css+ layer.name.name))
                  case false =>
                    println("Not Empty")
                    val s = mutable.Stack(layer.members: _*)
                    val nh = hashMap.+=((current + 1, s))
                    factorialAcc(current + 1, nh, ParseResult( parseResult.html + "<div>" + layer.name.name, parseResult.css +layer.name.name))
                }
              case sy: CommentSymbol =>
                factorialAcc(current - 1, hashMap, parseResult)
              case sy: Symbol =>
                println("otherSymbole")
                println("other: " + sy)
                factorialAcc(current - 1, hashMap, ParseResult(parseResult.html + "<div>" + sy.name.name + "</div>" + "</div>",parseResult.css+sy.name.name))
            }
          case a: Int if a > 1 =>
            println("hashMap.length > 1")
            val layer = hashMap(current).pop()
            layer match {
              case comment: CommentSymbol =>
                factorialAcc(current, hashMap, parseResult)
              case layer: LayerSymbol =>
               factorialAcc(current, hashMap, ParseResult( parseResult.html + "<div>" + layer.name.name + "</div>" + "</div>",parseResult.css+layer.name.name))
              case layer: PageSymbol =>
                factorialAcc(current, hashMap, ParseResult( parseResult.html + "<div>" + layer.name.name + "</div>" + "</div>",parseResult.css+layer.name.name))
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
    factorialAcc(1, hm, ParseResult("",""))
  }
}

