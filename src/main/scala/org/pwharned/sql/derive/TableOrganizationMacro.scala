package org.pwharned.sql.derive

import scala.quoted.*
import scala.io.Source
import java.io.InputStream
import scala.util.matching.Regex

object TableOrganizationMacro:

  /**
   * 1) Tests the regex on a hard-coded example  
   * 2) Reads the real DDL (if any)  
   * 3) Returns (tableName, isColumnOrganized)  
   */
  inline def tableInfo[T]: (String, Boolean) =
    ${ tableInfoImpl[T] }

  private def tableInfoImpl[T: Type](using q: Quotes): Expr[(String, Boolean)] =
    import q.reflect.*

    // 1) Derive the lowercase table name from the case-class symbol
    val tableName = TypeRepr.of[T].typeSymbol.name.toLowerCase

    // 2) Build the master regex with compile-time flags (?i)(?s)
    //    This version handles one level of nested parens (for vector(…, …))
    val nestedParens = """\((?:[^()]|\([^()]*\))*\)"""
    val patternSource =
      s"""(?is)                             # dot-all & case-insensitive
         \\bCREATE\\s+TABLE\\s+            # “CREATE TABLE”
         (?:\"[^\"]+\"|\\w+)?\\.?           # optional schema (quoted or bare)
         $tableName\\b\\s*                 # exact table name
         $nestedParens                     # (…) column list
         \\s*(?:ORGANIZED|ORGANIZE)\\s+BY\\s+(COLUMN|ROW)\\b
      """.stripMargin
        // remove comments/whitespace before giving to the JVM regex engine
        .replaceAll("""(?m)\s*#.*""", "")
        .replaceAll("\\s+", "")

    // 3) Compile the regex right now
    val tablePattern: Regex =
      try patternSource.r
      catch
        case ex: Throwable =>
          report.errorAndAbort(s"Invalid tablePattern regex: ${ex.getMessage}\n$patternSource")

    // 4) A built-in snippet we expect to match at least for “ORGANIZE BY COLUMN”
    val testSchema =
      s"""|CREATE TABLE portfolio.$tableName(
         |  id int not null,
         |  embedding vector(768, FLOAT32)
         |) ORGANIZE BY COLUMN;
         |""".stripMargin

    // 5) Run the test and abort compilation if it fails
    if tablePattern.findFirstMatchIn(testSchema).isEmpty then
      report.errorAndAbort(
        s"""Regex failed to match the built-in example for '$tableName'.
           |Pattern was: $patternSource
         """.stripMargin
      )

    // 6) Attempt to read the actual schema.sql (optional)
    val isColOrganized =   Option(classOf[TableOrganizationMacro.type].getResourceAsStream("/schema.sql")) match
      case Some(stream) =>
        try
          val ddl = Source.fromInputStream(stream).mkString
          tablePattern
            .findFirstMatchIn(ddl)
            .exists(_.group(1).equalsIgnoreCase("COLUMN"))
        finally
          stream.close()

      case None =>
        // No schema file → assume row-organized
        report.warning(s"schema.sql not found; defaulting '$tableName' to ROW-organized")
        false

    // 7) Spit out the final tuple
    Expr((tableName, isColOrganized))


  /** Convenience inline to get just the boolean flag */
  inline def isColumnOrganized[T]: Boolean =
    ${ isColumnOrganizedImpl[T] }

  private def isColumnOrganizedImpl[T: Type](using q: Quotes): Expr[Boolean] =
    val Expr((_, flag)) = tableInfoImpl[T]
    Expr(flag)
