package org.pwharned.generator
import org.pwharned.{Column, SQLParser}
import org.pwharned.SQLParser.{ColumnOps, alterTablePrimaryKeyParser}

import java.io.File
import java.nio.CharBuffer
import scala.io.Source

//case class User[F[_]](id: F[PrimaryKey[Int]], name: F[String])
object CaseClassGenerator {
  def generateCaseClasses(
      filePath: String,
      hkd: Boolean = true,
      tuples: Boolean = true,
      addTapirSchema: Boolean,
      addJsoniterCodec: Boolean
  ): String = {
    val input = Source.fromFile(filePath)("UTF-8")

    val file = new File(filePath)
    if (!file.exists() || !file.isFile) {
      throw new IllegalArgumentException(s"Schema file not found at: $filePath")
    }

    val lines = input.getLines().mkString("\n")
    val statements = lines.split(";")

    val createTableStatements = statements
      .filter(x => x.trim.toUpperCase.startsWith("CREATE TABLE"))
      .map(x => SQLParser.createTableParser(x))
    val alterTableStatements = statements
      .filter(x => x.trim.toUpperCase.startsWith("ALTER TABLE"))
      .map(x => SQLParser.alterTablePrimaryKeyParser(x))
    val alterColumn = statements
      .filter(x => x.trim.toUpperCase.startsWith("ALTER TABLE"))
      .map(x => SQLParser.alterTableAddGeneratedAlwaysAsIdentity(x))
    val comments = statements
      .filter(x => x.trim.toUpperCase.startsWith("COMMENT"))
      .map(x => SQLParser.commentParser(x))
    createTableStatements.map {
      case Left(value) => { System.out.println(value) }
      case Right(value) =>
        val alterations = alterTableStatements
          .filter { x =>
            x.isRight
          }
          .map(x => x.toOption.get)
          .filter(x => x._1.table.toUpperCase == value._1.name.toUpperCase)
        val c = comments
          .filter { x =>
            x.isRight
          }
          .map(x => x.toOption.get)
          .filter(x => x._1.table.toUpperCase == value._1.name.toUpperCase)

        val generated_always = alterColumn
          .filter { x =>
            x.isRight
          }
          .map(x => x.toOption.get)
          .filter(x => x._1.tableName.toUpperCase == value._1.name.toUpperCase)

        val columns = value._1.columns.map(x => {
          val generated =
            generated_always.find(y => y._1.colName == x.name) match {
              case Some(value) => true
              case None        => false

            }
          val comment = c.find(y => y._1.column.getOrElse("") == x.name)
          alterations.find(y => y._1.columns.contains(x.name)) match {
            case Some(value) =>
              Column(
                x.name,
                x.dataType,
                x.nullable,
                Some(true),
                Some(generated),
                x.default,
                comment.map(x => x._1)
              )
            case None => x
          }

        })
        println(value)

        val typeOrTuple = tuples match {
          case true  => "type"
          case false => "case class"
        }
        val tupleEquals = tuples match {
          case true  => "="
          case false => ""
        }

        hkd match {
          case true =>
            s"""
                |$typeOrTuple ${value._1.name}[F[_]] $tupleEquals (${columns
                .map(x => x.toField)
                .mkString(",\n")})
                object ${value._1.name} {
                  val docs = Map("comment" -> "${c
                .find(x => x._1.column.isEmpty)
                .map(x => x._1.comment)
                .getOrElse("")}", ${columns
                .map(x => s"${x.name} -> ${x.comment.getOrElse("")} ")
                .mkString(",")}
                ${
                if (addJsoniterCodec) {
                  s"given JsonValueCodec[${value._1.name}] = JsonCodecMaker.make"
                }
              }
                ${
                if (addTapirSchema) {
                  s"given Schema[${value._1.name}] = Schema.derived"
                }
              }
                }
                  
                """.stripMargin

          case false =>
            s"""
                            |$typeOrTuple ${value._1.name} $tupleEquals (${columns
                .map(x => x.toFieldLower)
                .mkString(",\n")})""".stripMargin
        }

    }

  }.mkString("\n")
}
