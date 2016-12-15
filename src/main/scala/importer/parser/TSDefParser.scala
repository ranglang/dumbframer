/* TypeScript importer for Scala.js
 * Copyright 2013-2014 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package importer.parser

import importer.Trees._

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.token._
import scala.util.parsing.input._

class TSDefParser extends StdTokenParsers with ImplicitConversions {

  type Tokens = StdTokens
  val lexical: TSDefLexical = new TSDefLexical

  lexical.reserved ++= List(
    // Value keywords
    "true", "false",
    // Additional keywords of FramerJs
    "Layer",    "width",    "x",    "y",    "html",    "style",
    "parent",    "height",    "backgroundColor",    "Align",    "center",    "left",
    "top",    "point",    "size",    "right",    "image",    "Import", "file",    "PageComponent", "Framer", "Importer", "load"
    , "on", "Events", "event", "layer", "Click", "addPage",
    "snapToPage", "new", "visible", "height", "scrollVertical", "clip",
    "title", "author", "twitter", "description", "Info"
  )

  lexical.delimiters ++= List(
    "{", "}", "(", ")", "[", "]", "<", ">",
    ".", ";", ",", "?", ":", "=", "|",
    "-", "+",
    // TypeScript-specific
    "...", "=>", "->",
    "#"
  )

  def parseDefinitions(input: Reader[Char]) =
    phrase(rep(moduleElementDecl1))(new lexical.Scanner(input))

  lazy val EventDecl: Parser[DeclTree] =
    identifier ~ ("." ~> "on" ~> "Events" ~> "." ~> "Click" ~> "," ~> "(" ~> "event" ~> "," ~> "layer" ~> ")" ~> "->" ~> rep(setProgressDecl)) ^^ EventIdent


  lazy val moduleElementDecl1: Parser[DeclTree] = (
    framerLayerDecl |
      framerPageDecl |
      annotationDecl |
      EventDecl |
      addPageDecl |
      snapToDecl |
      framerImporterDecl |
      setProgressDecl |
      frameInfoDecl
//      sketch.login.parent = pageLogin
    )

  lazy val frameInfoDecl: Parser[DeclTree] =
    ("Framer" ~> "." ~> "Info" ~> "=" ~> "title" ~> ":" ~> stringLit) ~ ("author" ~> ":" ~> stringLit) ~ ("twitter" ~> ":" ~> stringLit) ~ ("description" ~> ":" ~> stringLit) ^^ FramerInfo

  lazy val framerImporterDecl: Parser[DeclTree] =
    identifier ~> "=" ~> "Framer" ~> "." ~> "Importer" ~> "." ~> "load" ~> "(" ~> stringLiteral ~> ")" ^^ ImportFileIdent

  lazy val setProgressDecl: Parser[DeclTree] =
    repsep(identifier, ".") ~ ("=" ~> numericLit) ^^ SetProgressIdent

  lazy val annotationDecl: Parser[DeclTree] =
    "#" ~> "Import" ~> "file" ~> stringLit ~ framerImporterDecl ^^ AnnotationIdent

  lazy val framerLayerDecl: Parser[DeclTree] =
    identifier ~ ("=" ~> "new" ~> "Layer" ~> parameterBody) ^^ LayerDecl

  lazy val framerPageDecl: Parser[DeclTree] =
    identifier ~ ("=" ~> "new" ~> "PageComponent" ~> parameterBody) ^^ PageDecl

  lazy val snapToDecl: Parser[DeclTree] =
    identifier ~ ("." ~> "snapToPage" ~> "(" ~> identifier <~ ")") ^^ SnapToIdent

  lazy val parameterBody: Parser[List[TermTree]] =
    rep(paraType)


  lazy val paraType: Parser[TermTree] =
    "backgroundColor" ~> ":" ~> stringLit ^^ BackGroundColorIdent |
      "x" ~> ":" ~> valueDecl ^^ XIdent |
      "y" ~> ":" ~> valueDecl ^^ YIdent |
      "borderRadius" ~> ":" ~> valueDecl ^^ BorderRadiusIdent |
      "borderWidth" ~> ":" ~> valueDecl ^^ BorderWidthIdent |
      "point" ~> ":" ~> "Align" ~> "." ~> ("center" | "left" | "right") ^^ PointIdent |
      "visible" ~> ":" ~> identifier ^^ VisibleIdent |
      "image" ~> ":" ~> stringLit ^^ ImageIdent |
      "html" ~> ":" ~> stringLit ^^ HtmlIdent |
      "parent" ~> ":" ~> identifier ^^ ParentIdent |
      "width" ~> ":" ~> widthValueDecl ^^ WidthIdent |
      ("scrollVertical" ~> ":") ~> ("true" | "false") ^^ ScrollVerticalIdent |
      ("clip" ~> ":") ~> ("true" | "false") ^^ ClipIdent |
      "height" ~> ":" ~> widthValueDecl ^^ HeightIdent |
      "size" ~> ":" ~> (identifier <~ "." <~ "size") ^^ SizeIdent |
      "style" ~> ":" ~> styleParaType

  lazy val styleParaType: Parser[TermTree] =
    stringLit ~ (":" ~> stringLit) ^^ StyleIdent


  lazy val addPageDecl: Parser[DeclTree] =
    (identifier <~ "." <~ "addPage" <~ "(") ~ (identifier <~ ",") ~ stringLit <~ ")" ^^ AddPageIdent

  lazy val widthValueDecl: Parser[ValueTree] =
    (numericLit ^^ ValueIdent |
      (identifier <~ ".") ~ ("width" | "height") ^^ ValueWithIdent
      )

  lazy val valueDecl: Parser[ValueTree] =
    (numericLit ^^ ValueIdent |
      "Align" ~> "." ~> ("center" | "left" | "right" | "top") ~ opt("-" | "+") ~ opt(numericLit) ^^ Value3Ident
      )

  lazy val identifier =
    identifierName ^^ Ident

  lazy val identifierName =
    accept("IdentifierName", {
      case lexical.Identifier(chars) => chars
      case lexical.Keyword(chars) if chars.forall(Character.isLetter) => chars
      case a: Elem => {
        ""
      }
    })

  lazy val propertyName: Parser[PropertyName] =
    identifier | stringLiteral

  lazy val stringLiteral: Parser[StringLiteral] =
    stringLit ^^ StringLiteral
}
