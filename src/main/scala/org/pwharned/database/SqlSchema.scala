package org.pwharned.database

import org.pwharned.database.{DbTypeMapper, mapToSqlTypes}
import scala.language.implicitConversions
import scala.compiletime.*
import scala.deriving.Mirror

trait SqlSchema[T]:
  def createTable(mapper: DbTypeMapper): String

object SqlSchema:
  inline given derived[T <: Product](using m: Mirror.ProductOf[T]): SqlSchema[T] =
    new SqlSchema[T]:
      def createTable(mapper: DbTypeMapper): String =
        val tableName = constValue[m.MirroredLabel]
        val fieldNames = constValueTuple[m.MirroredElemLabels].productIterator.toList.map(_.toString)
        val fieldTypes = mapToSqlTypes[m.MirroredElemTypes](mapper)

        val columns = fieldNames.zip(fieldTypes).map { case (name, sqlType) => s"$name $sqlType" }
        val table = s"CREATE TABLE IF NOT EXISTS $tableName (\n  ${columns.mkString(",\n  ")}\n);"
        println(table)
        table
extension[T: SqlSchema] (t: T) def createTableStatement(using db:DbTypeMapper): String = summon[SqlSchema[T]].createTable(db)
