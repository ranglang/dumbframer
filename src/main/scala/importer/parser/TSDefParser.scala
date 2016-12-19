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
    "Layer", "width", "x", "y", "html", "style",
    "parent", "height", "backgroundColor", "Align", "center", "left",
    "top", "point", "size", "right", "image", "Import", "file", "PageComponent", "Framer", "Importer", "load"
    , "on", "Events", "event", "layer", "Click", "addPage",
    "snapToPage", "new", "visible", "height", "scrollVertical", "clip",
    "title", "author", "twitter", "description", "Info",
    "borderWidth","textAlign","center","fontSize","textHeight","color"
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
      EventDecl |
      addPageDecl |
      snapToDecl |
      setProgressDecl |
      setParentDecl |
      frameInfoDecl |
      framerImporterDecl |
      setVisibleDecl
    )

  lazy val frameInfoDecl: Parser[DeclTree] =
    ("Framer" ~> "." ~> "Info" ~> "=" ~> "title" ~> ":" ~> stringLit) ~ ("author" ~> ":" ~> stringLit) ~ ("twitter" ~> ":" ~> stringLit) ~ ("description" ~> ":" ~> stringLit) ^^ FramerInfo

  lazy val framerImporterDecl: Parser[DeclTree] =
    identifier ~> "=" ~> "Framer" ~> "." ~> "Importer" ~> "." ~> "load" ~> "(" ~> stringLit ~> ")" ^^ ImportFileIdent

  lazy val setProgressDecl: Parser[DeclTree] =
    repsep(identifier, ".") ~ ("=" ~> numericLit) ^^ SetProgressIdent

  lazy val setParentDecl: Parser[DeclTree] =
    repsep(identifier, ".") ~ ("parent" ~> "=" ~> identifier) ^^ SetParentIdent

  lazy val setVisibleDecl: Parser[DeclTree] =
    rep1(rep(identifier <~ "."), "visible") ~ ("=" ~> valueDecl) ^^ SetVisibleIdent


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
      "visible" ~> ":" ~>
        ("true" | "false") ^^ {
        case "true" => true
        case "false" => false
      } ^^ VisibleIdent |
      "image" ~> ":" ~> stringLit ^^ ImageIdent |
      "html" ~> ":" ~> stringLit ^^ HtmlIdent |
      "parent" ~> ":" ~> identifier ^^ ParentIdent |
      "width" ~> ":" ~> valueDecl ^^ WidthIdent |
       "height" ~> ":" ~> valueDecl ^^ HeightIdent |
      ("scrollVertical" ~> ":") ~> valueDecl ^^ ScrollVerticalIdent |
      ("clip" ~> ":") ~> ("true" | "false") ^^ ClipIdent |
      "size" ~> ":" ~> (identifier <~ "." <~ "size") ^^ SizeIdent |
      "style" ~> ":" ^^ EmptyIdent |
      ("textAlign" ~> ":"~> stringLit) ^^ StyleTextAlignIdent |
      ("fontSize" ~> ":"~> stringLit) ^^ StyleFontSizeIdent |
      ("textHeight" ~> ":"~> stringLit) ^^ TextHeightIdent |
      ("color" ~> ":"~> stringLit) ^^ FontColorIdent


  lazy val addPageDecl: Parser[DeclTree] =
    (identifier <~ "." <~ "addPage" <~ "(") ~ (identifier <~ ",") ~ stringLit <~ ")" ^^ AddPageIdent

  lazy val valueDecl: Parser[ValueTree] =
    (numericLit ^^ StringIdent |
      ("true" | "false") ^^ {
        case "true" => true
        case "false" => false
      } ^^ BooleanValueIdent |
      "Align" ~> "." ~> ("center" | "left" | "right" | "top") ~ opt("-" | "+") ~ opt(numericLit) ^^ Value3Ident |
      (identifier <~ ".") ~ ("width" | "height") ^^ ValueWithIdent
      )

  lazy val identifier =
    identifierName ^^ Ident

  lazy val identifierName =
    accept("IdentifierName", {
      case lexical.Identifier(chars) => chars
      case lexical.Keyword(chars) if chars.forall(Character.isLetter) => chars
      case _ => {
        ""
      }
    })
}
