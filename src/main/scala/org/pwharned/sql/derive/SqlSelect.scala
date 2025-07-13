package org.pwharned.sql.derive

import org.pwharned.sql.database.{Rs, summonFieldTypes}
import org.pwharned.sql.statements.{ SelectStatement}

import scala.compiletime.*
import scala.concurrent.{ExecutionContext, Future}
import scala.deriving.*
import scala.language.implicitConversions

trait SqlSelect[T] {
  def names: List[String]
  def name: String
  def select: String
  def selectWhere: String
  def selectWhere(ob:T): String

}



object SqlSelect:

  inline given derived[T <: Product](using m: Mirror.ProductOf[T]): SqlSelect[T] = {
    new SqlSelect[T] {
      def name: String = constValue[m.MirroredLabel]

      def names: List[String] =
        constValueTuple[m.MirroredElemLabels].productIterator.toList.map(_.toString)

      def select: String =   summonInline[SelectStatement[T]].select



      def selectWhere(obj: T): String =
        val tableName = constValue[m.MirroredLabel]
        val fields = constValueTuple[m.MirroredElemLabels].toList.map(_.toString)
        // Extract values using productIterator
        val values = obj.productIterator.toList

        // Filter out fields with None or null values
        val where = fields.zip(values).collect {
          case (name, value) if value != None => s"$name = ?"
        }.mkString(" and ")


        val sql = s"SELECT ${fields.mkString(",")} from $tableName where $where  ;"
        sql

      def selectWhere: String = {
        val tableName = constValue[m.MirroredLabel]
        val fields = constValueTuple[m.MirroredElemLabels].toList.map(_.toString)

        val primaryKey = PrimaryKeyExtractor.getPrimaryKey[T].map(x => s" $x = ? ").mkString(" AND ")

        val sql = s"SELECT ${fields.mkString(",")} from $tableName WHERE $primaryKey"
        sql

      }



    }
  }

extension [T<:Product](entity: T)(using sql: SqlSelect[T])
  def fields: List[String] = summon[SqlSelect[T]].names
  def select: String = summon[SqlSelect[T]].select

