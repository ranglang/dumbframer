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
    var s: Set[Symbol] = Set()
    @tailrec def factorialAcc(slice: mutable.Stack[Symbol], head: String): String = {
      if (slice.isEmpty) head
      else {
        val sym = slice.head
        sym match {
          case sym: PackageSymbol =>
            slice.pop();
            if (!sym.members.isEmpty) {
              factorialAcc(slice.pushAll(sym.members), head)
            } else
              head
          case PageSymbol(name, params, list) =>
            slice.pop();
            head
          case layer: LayerSymbol =>
            if (s(layer)) {
              factorialAcc(slice.drop(1), head + layer.name.name)
            } else {
              s = s.+(layer)
              if (!layer.members.isEmpty) {
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
          case layer: NotSupportSymbol =>
            layer.value
          case a: Symbol =>
            head
        }
      }
    }
    factorialAcc(mutable.Stack(initialSym), head)
  }
}

