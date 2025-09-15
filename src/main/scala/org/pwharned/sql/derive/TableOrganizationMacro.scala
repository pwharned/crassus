// TableOrganizationMacro.scala
package org.pwharned.sql.derive

import scala.quoted.*
import scala.io.Source
import java.io.InputStream

object TableOrganizationMacro:
  inline def tableInfo[T]: (String, Boolean) =
    ${ tableInfoImpl[T] }

  def tableInfoImpl[T: Type](using Quotes): Expr[(String, Boolean)] =
    import quotes.reflect.*

    // Extract the case class name (which is the table name)
    val tableName = TypeRepr.of[T].typeSymbol.name.toLowerCase
    val ddlContent = readDDLFile()
    val isColOrganized = parseTableOrganization(ddlContent, tableName)

    Expr((tableName, isColOrganized))
  inline def isColumnOrganized[T]: Boolean =
    ${ isColumnOrganizedImpl[T] }

  def isColumnOrganizedImpl[T: Type](using Quotes): Expr[Boolean] =
    import quotes.reflect.*

    // Extract the case class name (which is the table name)
    val tableName = TypeRepr.of[T].typeSymbol.name
    val ddlContent = readDDLFile()
    val isColOrganized = parseTableOrganization(ddlContent, tableName)

    Expr(isColOrganized)

  private def readDDLFile()(using Quotes): String =
    val resourcePath = "/schema.sql"
    val stream: InputStream = getClass.getResourceAsStream(resourcePath)
    if (stream == null) then
      quotes.reflect.report.errorAndAbort(s"DDL file not found at $resourcePath")

    try
      Source.fromInputStream(stream).mkString
    finally
      stream.close()

  private def parseTableOrganization(ddlContent: String, tableName: String): Boolean =
    // Case insensitive regex to find CREATE TABLE statements and check organization
    val tablePattern = s"(?i)CREATE\\s+TABLE\\s+(?:\\w+\\.)?$tableName\\s*\\([^)]*\\)\\s*ORGANIZED\\s+BY\\s+(COLUMN|ROW)".r

    tablePattern.findFirstMatchIn(ddlContent) match
      case Some(matched) =>
        matched.group(1).toUpperCase == "COLUMN"
      case None =>
        false // Default to row-organized if not specified
