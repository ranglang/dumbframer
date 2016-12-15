/* TypeScript importer for Scala.js
 * Copyright 2013-2014 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package importer

import java.io.{Console => _, Reader => _, _}

import importer.Trees._
import importer.parser.TSDefParser

import scala.util.parsing.input.{PagedSeqReader, Reader}

/** Entry point for the TypeScript importer of Scala.js */
object FramerParser {
  def parse(s: PagedSeqReader): String = {
    val parser = new TSDefParser
    parser.parseDefinitions(s) match {
      case parser.Success(rawCode: List[DeclTree], _) =>
      new Importer().parse(rawCode, "")
      case parser.NoSuccess(msg, next) =>
        "Parse error at %s\n".format(next.pos.toString) +
          msg + "\n" +
          next.pos.longString
    }
  }
}