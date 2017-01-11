/* TypeScript importer for Scala.js
 * Copyright 2013-2014 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package importer.sc

import importer.Trees.TermTree
import importer.Utils

import scala.annotation.tailrec
import scala.collection.mutable._
import scala.language.implicitConversions

case class Name(name: String) {
  override def toString() = Utils.scalaEscape(name)
}

object Name {
  val EMPTY = Name("")
}

class Symbol(val name: Name) {
  override def toString() =
    s"${this.getClass.getSimpleName}($name)}"
}

class CommentSymbol(val text: String) extends Symbol(Name("<comment>")) {
  override def toString() =
    s"/* $text */"
}

class ContainerSymbol(nme: Name) extends Symbol(nme) {
  val members = new ListBuffer[Symbol]

  private var _anonMemberCounter = 0
  def newAnonMemberName() = {
    _anonMemberCounter += 1
    "anon$" + _anonMemberCounter
  }

  def getLayer(name: Name): Symbol = {
    @tailrec def get(list: ListBuffer[Symbol], ret: ListBuffer[Symbol]) :ListBuffer[Symbol]= {
      if( list.isEmpty)
        ret
      else {
        val current = list.head
         current match {
          case LayerSymbol(name,params,members,opts) =>  get(list.drop(1).++(members), ret+=current)
          case PageSymbol(name,params, members) => get(list.drop(1).++(members), ret+=current)
          case _ => get(list.drop(1),ret)
        }
      }
    }

    val b = get(members, ListBuffer.empty[Symbol]);
      val optMembers = b.collectFirst {
        case sym: LayerSymbol if sym.name == name => sym
        case sym1: PageSymbol if sym1.name == name => sym1
      }
    optMembers.get
  }
}

case class PackageSymbol(nme: Name) extends ContainerSymbol(nme) {
  override def toString() = s"package $name"
}

case class TextSymbol(nme: Name, value: String, params: List[TermTree], parentOpt: Option[String]) extends Symbol(nme) {
}

case class InputSymbol(nme: Name,inputType: String, value: String, params: List[TermTree], parentOpt: Option[String]) extends Symbol(nme) {
}

case class ImageSymbol(nme: Name, value: String,imageUrl: String, params: List[TermTree], parentOpt: Option[String]) extends Symbol(nme) {
}

case class NotSupportSymbol(nme: Name, value: String) extends Symbol(nme) {
}
case class LayerSymbol(nme: Name, params: List[TermTree], members: ListBuffer[Symbol] = ListBuffer(), parentOpt: Option[String]) extends Symbol(nme) {
}

case class ScrollerLayerSymbol(nme: Name, params: List[TermTree], members: ListBuffer[Symbol] = ListBuffer(), parentOpt: Option[String]) extends Symbol(nme) {
}

case class PageSymbol(nme: Name, params: List[TermTree], members: ListBuffer[Symbol] = ListBuffer()) extends Symbol(nme) {
}

