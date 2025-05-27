package org.pwharned.macros

import scala.deriving.Mirror
import scala.compiletime.*

trait SqlSchema[T]:
  def createTable(mapper: DbTypeMapper): String

object SqlSchema:
  inline given derived[T <: Product](using m: Mirror.ProductOf[T]): SqlSchema[T] =
    new SqlSchema[T]:
      def createTable(mapper: DbTypeMapper): String =
        val tableName = constValue[m.MirroredLabel]
        val fieldNames = constValueTuple[m.MirroredElemLabels].toIArray.toList.map(_.toString)
        val fieldTypes = mapToSqlTypes[m.MirroredElemTypes](mapper)

        val columns = fieldNames.zip(fieldTypes).map { case (name, sqlType) => s"$name $sqlType" }
        s"CREATE TABLE IF NOT EXISTS $tableName (\n  ${columns.mkString(",\n  ")}\n);"
