/* TypeScript importer for Scala.js
 * Copyright 2013-2014 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package tsimporter

import java.io.{Console => _, Reader => _, _}

import tsimporter.Trees._
import tsimporter.parser.TSDefParser

import scala.collection.immutable.PagedSeq
import scala.util.parsing.input.{PagedSeqReader, Reader}

/** Entry point for the TypeScript importer of Scala.js */
object Main {

  def a(s: PagedSeqReader): String = {
    val outputPackage = ""

    val definitions = parseDefinitions(s)
    new Importer().a(definitions, outputPackage)
  }

  private def parseDefinitions(reader: Reader[Char]): List[DeclTree] = {
    val parser = new TSDefParser
    parser.parseDefinitions(reader) match {
      case parser.Success(rawCode, _) =>
        rawCode

      case parser.NoSuccess(msg, next) =>
        Console.err.println(
          "Parse error at %s\n".format(next.pos.toString) +
            msg + "\n" +
            next.pos.longString)
        sys.exit(2)
    }
  }
}
