/* TypeScript importer for Scala.js
 * Copyright 2013-2014 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package tsimporter

import tsimporter.Trees._
import tsimporter.sc._

/** The meat and potatoes: the importer
 *  It reads the TypeScript AST and produces (hopefully) equivalent Scala
 *  code.
 */
class Importer(
              ) {

  import Importer._

  /** Entry point */
  //  def apply() {
  //
  //  }

  def a(declarations: List[DeclTree], outputPackage: String): String = {
    val rootPackage = new PackageSymbol(Name.EMPTY)
    for (declaration <- declarations)
      processDecl(rootPackage, declaration)

    new Printer(// output, output1,
      outputPackage).printSymbol(rootPackage, "")
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

        val contaner = htmlOpt match {
          case Some(html) => new TextSymbol(name, html.value, params)
          case None => new LayerSymbol(name, params);
        }

        parentOpt match {
          case Some(parentIdent) =>
            owner.getLayer(Name(parentIdent.value.name)).members += contaner
          case None =>
            owner.members += contaner
        }
      case AnnotationIdent(file, filePath) =>
      case EventIdent(ident, progress) =>
      case SnapToIdent(source, target) =>
      case AddPageIdent(ident, value, position) =>
      case SetProgressIdent(ident, value) =>
      case _ =>
        owner.members += new CommentSymbol("??? " + declaration)
    }
  }

//  private def processMembersDecls(enclosing: ContainerSymbol,
//                                  owner: ContainerSymbol, members: List[MemberTree]) {

//    val OwnerName = owner.name

    //    lazy val companionClassRef = {
    //      val tparams = enclosing.findClass(OwnerName) match {
    //        case Some(clazz) =>
    //          clazz.tparams.toList.map(tp => TypeRefTree(TypeNameName(tp.name), Nil))
    //        case _ => Nil
    //      }
    //      TypeRefTree(TypeNameName(OwnerName), tparams)
    //    }

//    for (member <- members) member match {
      //      case CallMember(signature) =>
      //        processDefDecl(owner, Name("apply"), signature, protectName = false)
      //
      //      case ConstructorMember(sig @ FunSignature(tparamsIgnored, params, Some(resultType)))
      //      if owner.isInstanceOf[ModuleSymbol] && resultType == companionClassRef =>
      //        val classSym = enclosing.getClassOrCreate(owner.name)
      //        classSym.isTrait = false
      //        processDefDecl(classSym, Name.CONSTRUCTOR,
      //            FunSignature(Nil, params, Some(TypeRefTree(CoreType("void")))))


//      case _ =>
//        Console.println(members)
//        owner.members += new CommentSymbol("??? " + member)
//    }
//  }

}


object Importer {
  private object IdentName {
    @inline def unapply(ident: Ident) =
      Some(Name(ident.name))
  }


  private object PropertyNameName {
    @inline def unapply(propName: PropertyName) =
      Some(Name(propName.name))
  }
}
