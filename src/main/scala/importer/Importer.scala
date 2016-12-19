/* TypeScript importer for Scala.js
 * Copyright 2013-2014 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package importer

import importer.Trees._
import importer.sc._

import scala.collection.mutable.ListBuffer

/** The meat and potatoes: the importer
 *  It reads the TypeScript AST and produces (hopefully) equivalent Scala
 *  code.
 */
class Importer() {
  import Importer._
  def parse(declarations: List[DeclTree], outputPackage: String): ParseResult = {
    val rootPackage = new PackageSymbol(Name.EMPTY)
    for (declaration <- declarations)
      processDecl(rootPackage, declaration)
     Printer.printSymbol(rootPackage)
  }

  private def processDecl(owner: ContainerSymbol, declaration: DeclTree) {
    declaration match {
      case PageDecl(IdentName(name), params) =>
        owner.members += new PageSymbol(name, params)
      case LayerDecl(IdentName(name), params) =>
        params.collectFirst {
          case cat: HtmlIdent => cat
        } match {
          case Some(html) => {
            params.collectFirst{
              case str: ParentIdent => str
            } match {
              case Some(parentIdent) =>
                val container = new TextSymbol(name, html.name, params,Some(parentIdent.value.name))
                owner.getLayer(Name(parentIdent.value.name)).members += container
              case None =>
                val container = new TextSymbol(name, html.name, params,None)
                owner.members += container
            }
          }
          case None => {
            params.collectFirst{
              case str: ParentIdent => str
            } match {
              case Some(parentIdent) =>
                  owner.getLayer(Name(parentIdent.value.name)).members += new LayerSymbol(name, params,ListBuffer(), Some(parentIdent.value.name));
              case  None =>
                owner.members += new LayerSymbol(name, params,ListBuffer(),Option.empty[String]);
            }
          }
         }
      case AnnotationIdent(file, filePath) =>
      case EventIdent(ident, progress) =>
      case SnapToIdent(source, target) =>
      case AddPageIdent(ident, value, position) =>
      case SetProgressIdent(ident, value) =>
      case NotSupportedDecl(msg) =>
        owner.members += new NotSupportSymbol(Name.EMPTY,msg)
      case _ =>
        owner.members += new CommentSymbol("??? " + declaration)
    }
  }
}


object Importer {
  private object IdentName {
    @inline def unapply(ident: Ident) =
      Some(Name(ident.name))
  }
}
