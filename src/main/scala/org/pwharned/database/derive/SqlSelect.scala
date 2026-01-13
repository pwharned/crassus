package org.pwharned.database.derive

import scala.compiletime.*
import scala.concurrent.{ExecutionContext, Future}
import scala.deriving.*
import scala.language.implicitConversions
trait SqlSelect[T] {

  def select: String
  def selectWhere: String
  def selectWhere(ob: T): String

}

object SqlSelect:

  inline given derived[T <: Product](using
      m: Mirror.ProductOf[T]
  ): SqlSelect[T] = {
    val tableName: String = summon[SqlTableName[T]].name()
    val schemaName: Option[String] = summon[SqlSchemaName[T]].schema()
    val name = schemaName match
      case Some(value) => s"${value}.${tableName}"
      case None        => tableName

    val names: List[String] =
      constValueTuple[m.MirroredElemLabels].productIterator.toList
        .map(_.toString)

    val selectStatement: String = summonInline[SelectStatement[T]].select()

    new SqlSelect[T] {

      def select: String = selectStatement

      def selectWhere(obj: T): String =
        val values = obj.productIterator.toList
        // Filter out fields with None or null values
        val where = names
          .zip(values)
          .collect {
            case (name, value) if value != None => s" $name = ? "
          }
          .mkString(" and ")

        val sql = s"$select where $where  "
        sql

      def selectWhere: String = {
        val pkNames = primaryKeyNames[T]

        val primaryKey = pkNames.map(x => s" $x = ? ").mkString(" AND ")

        val sql = s"SELECT ${names.mkString(",")} from $name WHERE $primaryKey"
        sql

      }

    }
  }

extension [T <: Product](entity: T)(using sql: SqlSelect[T])
  def select: String = summon[SqlSelect[T]].select
