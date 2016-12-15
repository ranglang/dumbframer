/* TypeScript importer for Scala.js
 * Copyright 2013-2014 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package importer

import importer.Trees._
import importer.sc._

/** The meat and potatoes: the importer
 *  It reads the TypeScript AST and produces (hopefully) equivalent Scala
 *  code.
 */
class Importer() {

  import Importer._

  def parse(declarations: List[DeclTree], outputPackage: String): String = {
    val rootPackage = new PackageSymbol(Name.EMPTY)
    for (declaration <- declarations)
      processDecl(rootPackage, declaration)
     Printer.printSymbol(rootPackage, "")
  }

  private def processDecl(owner: ContainerSymbol, declaration: DeclTree) {
    declaration match {
      case PageDecl(IdentName(name), params) =>
        owner.members += new PageSymbol(name, params)
      case LayerDecl(IdentName(name), params) =>
        val parentOpt: Option[ParentIdent] = params.collectFirst {
          case cat: ParentIdent => cat
        }
        val htmlOpt = params.collectFirst {
          case str: HtmlIdent => str
        }
        val container = htmlOpt match {
          case Some(html) => new TextSymbol(name, html.value, params)
          case None => new LayerSymbol(name, params);
        }

        parentOpt match {
          case Some(parentIdent) =>
            owner.getLayer(Name(parentIdent.value.name)).members += container
          case None =>
            owner.members += container
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
