package org.pwharned.sql.derive

import scala.quoted.*
import scala.io.Source
import scala.util.matching.Regex
import java.io.InputStream

object TableOrganizationMacro:

  inline def tableInfo[T]: (String, Boolean) =
    ${ tableInfoImpl[T] }

  private def tableInfoImpl[T: Type](using q: Quotes): Expr[(String, Boolean)] =
    import q.reflect.*

    // 0) Helper: Peel away applied/higher-kinded wrappers
    def baseType(tpe: TypeRepr): TypeRepr = tpe.dealias match
      case AppliedType(tycon, _)   => baseType(tycon)
      case OrType(left, _)         => baseType(left)
      case AndType(left, _)        => baseType(left)
      case AnnotatedType(under, _) => baseType(under)
      case ByNameType(under)       => baseType(under)
      case TypeRef(_, _) | _       => tpe.dealias

    // 1) Extract the true table name symbol
    val rawRepr = TypeRepr.of[T]
    val unwrapped = baseType(rawRepr)
    val tableName = unwrapped.typeSymbol.name.toLowerCase

    // 2) Build & compile your regex (dot-all + case-insensitive)
    val nestedParens = """\((?:[^()]|\([^()]*\))*\)"""
    val patternSrc =
      s"(?is)\\bCREATE\\s+TABLE\\s+(?:\"[^\"]+\"|\\w+)?\\.?$tableName\\b\\s*$nestedParens\\s*(?:ORGANIZED|ORGANIZE)\\s+BY\\s+(COLUMN|ROW)\\b"
    val tablePattern: Regex =
      try patternSrc.r
      catch
        case ex =>
          report.errorAndAbort(
            s"Invalid regex for table '$tableName': ${ex.getMessage}\n$patternSrc"
          )

    // 3) Self-test on a minimal snippet using the same tableName
    val testSchema =
      s"""CREATE TABLE dummy.$tableName(
         |  id   int,
         |  vec  vector(2, FLOAT32)
         |) ORGANIZE BY COLUMN;
         |""".stripMargin
    if tablePattern.findFirstMatchIn(testSchema).isEmpty then
      report.errorAndAbort(
        s"""Regex failed to match builtin example for '$tableName'.
           |Pattern: $patternSrc
           |Example DDL: $testSchema
         """.stripMargin
      )

    // 4) Load YOUR app’s schema.sql via the compiler’s classloader
    val loader = Thread.currentThread().getContextClassLoader
    val schemaOpt: Option[String] =
      Option(loader.getResourceAsStream("schema.sql")).map { is =>
        try Source.fromInputStream(is).mkString
        finally is.close()
      }

    // 5) Apply the regex (or default to ROW)
    val isColumnOrganized = schemaOpt match
      case Some(ddl) =>
        tablePattern
          .findFirstMatchIn(ddl)
          .exists(_.group(1).equalsIgnoreCase("COLUMN"))
      case None =>
        report.warning(s"schema.sql not found; defaulting '$tableName' → ROW")
        false

    Expr((tableName, isColumnOrganized))
