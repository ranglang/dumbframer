/* TypeScript importer for Scala.js
 * Copyright 2013-2014 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package importer

import importer.Trees._
import importer.sc._
import utl.FramerConfig

import scala.collection.mutable.ListBuffer

/** The meat and potatoes: the importer
  * It reads the TypeScript AST and produces (hopefully) equivalent Scala
  * code.
  */
class Importer() {

  import Importer._

  def parseSome(declarations: List[DeclTree]): Symbol = {
    val rootPackage = new PackageSymbol(Name.EMPTY)
    for (declaration <- declarations)
      processDecl(rootPackage, declaration)
    rootPackage
  }

  def parse(declarations: List[DeclTree], outputPackage: String, projectPath: Option[String], framerConfig: FramerConfig): ParseResult = {
    val rootPackage = new PackageSymbol(Name.EMPTY)
    for (declaration <- declarations)
      processDecl(rootPackage, declaration)
    Printer.printSymbol(rootPackage, projectPath, framerConfig)
  }


 final def handleImageSymbol (owner: ContainerSymbol,layer: LayerDecl, image:ImageIdent) = {
    val name = Name(layer.name.name)
    val params = layer.members
    params.collectFirst {
      case str: ParentIdent => str
    } match {
      case Some(parentIdent) =>
        val container = new ImageSymbol(name, image.value, image.value, params, Some(parentIdent.value.name))
        owner.getLayer(Name(parentIdent.value.name)) match {
          case a: PageSymbol =>
            a.members += container
          case a: LayerSymbol =>
            a.members += container
        }
      case None =>
        val container = new ImageSymbol(name, image.value, image.value, params, None)
        owner.members += container
    }
  }

  final def handleTextSymbol (owner: ContainerSymbol,layer: LayerDecl, html: HtmlIdent) = {
    val name = Name(layer.name.name)
    val params = layer.members
    params.collectFirst {
      case str: ParentIdent => str
    } match {
      case Some(parentIdent) =>
        val container = new TextSymbol(name, html.value, params, Some(parentIdent.value.name))
        owner.getLayer(Name(parentIdent.value.name)) match {
          case a: PageSymbol =>
            a.members += container
          case a: LayerSymbol =>
            a.members += container
        }
      case None =>
        val container = new TextSymbol(name, html.value, params, None)
        owner.members += container
    }
  }

  final def handleLayerSymbol (owner: ContainerSymbol,layer: LayerDecl) = {
    val name = Name(layer.name.name)
    val params = layer.members
    params.collectFirst {
      case str: ParentIdent => str
    } match {
      case Some(parentIdent) =>
        val container = new LayerSymbol(name, params, ListBuffer(), Some(parentIdent.value.name));
        owner.getLayer(Name(parentIdent.value.name)) match {
          case a: PageSymbol =>
            a.members += container
          case a: LayerSymbol =>
            a.members += container
        }
      case None =>
        owner.members += new LayerSymbol(name, params, ListBuffer(), Option.empty[String]);
    }
  }

  private def processDecl(owner: ContainerSymbol, declaration: DeclTree) {
    declaration match {
      case PageDecl(IdentName(name), params) =>
        owner.members += new PageSymbol(name, params)
      case  LayerDecl(IdentName(name), params) =>
        params.collectFirst {
          case cat: ImageIdent => cat
        } match {
          case Some(image) => {
             handleImageSymbol (owner,LayerDecl(Ident(name.name),params), image)
          }
          case None =>
            params.collectFirst {
              case cat: HtmlIdent => cat
            } match {
              case Some(html) => {
                handleTextSymbol(owner, LayerDecl(Ident(name.name),params), html)
              }
              case None => {
                handleLayerSymbol(owner,LayerDecl(Ident(name.name),params) )
              }
            }
        }
      case AnnotationIdent(file, filePath) =>
      case EventIdent(ident, progress) =>
      case SnapToIdent(source, target) =>
      case AddPageIdent(ident, value, position) =>
      case SetProgressIdent(ident, value) =>
      case NotSupportedDecl(msg) =>
        owner.members += new NotSupportSymbol(Name.EMPTY, msg)
      case _ =>
        owner.members
    }
  }
}


object Importer {

  private object IdentName {
    @inline def unapply(ident: Ident) =
      Some(Name(ident.name))
  }

}
