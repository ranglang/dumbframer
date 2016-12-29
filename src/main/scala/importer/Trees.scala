/* TypeScript importer for Scala.js
 * Copyright 2013-2014 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package importer

import scala.util.parsing.input.Positional

object Trees {

  abstract sealed class Tree extends Positional {
  }

  sealed trait DeclTree extends Tree
  sealed trait TermTree extends Tree
  sealed trait ValueTree extends Tree

  sealed trait PropertyName extends TermTree {
    def name: String
  }

  case class AnnotationIdent(annotation: String, file: DeclTree) extends DeclTree with PropertyName {
    override def name: String = "annotation"
  }

  case class EventIdent(ident: Ident, members: List[DeclTree]) extends DeclTree with PropertyName {
    override def name: String = "size"
  }

  case class PointIdent(position: String) extends TermTree with PropertyName {
    override def name: String = "point"
  }

  case class SizeIdent(iden: Ident) extends TermTree with PropertyName {
    override def name: String = "size"
  }

  case class FramerInfo(title: String, info1: String, info2: String, info3: String) extends DeclTree with PropertyName {
    override def name: String = "framerInfo"
  }

  case class ImportFileIdent(annotation: String) extends DeclTree with PropertyName {
    override def name: String = "annotation"
  }

  case class StyleIdent(list: List[TermTree]) extends TermTree with PropertyName {
    override def name: String = "style"
  }

  case class EmptyIdent(str: String) extends TermTree

  case class SetProgressIdent(paraName: List[Ident], paraValue: String) extends DeclTree with PropertyName {
    override def name: String = "setProgress"
  }

  case class SetVisibleIdent(paraName: List[_], paraValue: ValueTree) extends DeclTree with PropertyName {
    override def name: String = "setVisible"
  }
  case class SetParentIdent(paraName: List[Ident], paraValue: Ident) extends DeclTree with PropertyName {
    override def name: String = "setParent"
  }

  case class SnapToIdent(source: Ident, target: Ident) extends DeclTree with PropertyName {
    override def name: String = "snapTo"
  }

  case class AddPageIdent(source: Ident, target: Ident, position: String) extends DeclTree with PropertyName {
    override def name: String = "addPage"
  }

  case class ImageIdent(value: String) extends TermTree with PropertyName {
    override def name: String = "image"
  }

  case class HtmlIdent(value: String) extends TermTree with PropertyName {
    override def name: String = "html"
  }

  case class ParentIdent(value: Ident) extends TermTree with PropertyName {
    override def name: String = "parent"
  }

  case class StyleTextAlignIdent(value: String) extends TermTree

  case class StyleFontSizeIdent(value: String) extends TermTree

  case class LineHeightIdent(value: String) extends TermTree
  case class FontColorIdent(value: String) extends TermTree

  case class BorderColorIdent(value: String) extends TermTree

  case class WidthIdent(value: ValueTree) extends TermTree

  case class BackGroundColorIdent(value: String) extends TermTree

  case class VisibleIdent(isVisible: Boolean) extends TermTree

  case class HeightIdent(value: ValueTree) extends TermTree

  case class BorderWidthIdent(value: ValueTree) extends TermTree

  case class BorderRadiusIdent(value: ValueTree) extends TermTree

  case class YIdent(value: ValueTree) extends TermTree
  case class XIdent(value: ValueTree) extends TermTree

  case class ScrollVerticalIdent(bool: ValueTree) extends TermTree

  case class ScrollHorizontal(bool: ValueTree) extends TermTree

  case class OpacityIdent(value: ValueTree) extends TermTree

  case class BorderTopLeftRadiusIdent(value: ValueTree) extends TermTree
  case class BorderTopRightRadiusIdent(value: ValueTree) extends TermTree
  case class BorderStyleIdent (value: ValueTree) extends TermTree

  case class ShadowSpreadIdent(value: ValueTree) extends TermTree
  case class ShadowColorIdent(value: ValueTree) extends TermTree
  case class ShadowYIdent(value: ValueTree) extends TermTree
  case class ShadowXIdent(value: ValueTree) extends TermTree
  case class ShadowBlurIdent(value: ValueTree) extends TermTree

  case class ClipIdent(bool: String) extends ValueTree with PropertyName {
    override def name: String = "scrollVertical"
  }

  case class NumberIdent(name: String) extends ValueTree with PropertyName {
  }

  case class StringIdent(name: String) extends ValueTree

  case class BooleanValueIdent(name: Boolean ) extends ValueTree

  case class Value3Ident(name: String, calculate: Option[String], num: Option[String]) extends ValueTree with PropertyName {
  }

  case class ValueWithIdent(content: Ident, value: String) extends ValueTree

  case class Ident(name: String) extends Tree with PropertyName {
    Ident.requireValidIdent(name)
  }


  object Ident extends (String => Ident) {
    final def isValidIdentifier(name: String): Boolean = {
      val c = name.head
      (c == '$' || c == '_' || c.isUnicodeIdentifierStart) &&
        name.tail.forall(c => c == '$' || c.isUnicodeIdentifierPart)
    }
    @inline final def requireValidIdent(name: String) {
      require(isValidIdentifier(name), s"${name} is not a valid identifier")
    }
  }

  case class WidthDecl(value: String) extends DeclTree
  case class LayerDecl(name: Ident, members: List[TermTree]) extends DeclTree
  case class PageDecl(name: Ident, members: List[TermTree]) extends DeclTree
  case class NotSupportedDecl(name: String) extends DeclTree
  sealed trait Literal extends TermTree

  sealed abstract class BaseTypeRef extends Tree
}

