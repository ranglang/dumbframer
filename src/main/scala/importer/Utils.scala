/* TypeScript importer for Scala.js
 * Copyright 2013-2014 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package importer

object Utils {

  def scalaEscape(ident: String): String =
    if (needsEscaping(ident)) "`" + ident + "`"
    else ident

  def needsEscaping(ident: String): Boolean = (
      ident.isEmpty ||
      (!ident.head.isUnicodeIdentifierStart && ident.head != '_') ||
      !ident.tail.forall(_.isUnicodeIdentifierPart)
  )

}
