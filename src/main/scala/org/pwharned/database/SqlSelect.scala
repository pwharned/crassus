package org.pwharned.database

import org.pwharned.database.summonFieldTypes
import HKD.*
import scala.language.implicitConversions
import scala.compiletime.*
import scala.concurrent.{ExecutionContext, Future}
import scala.deriving.*

trait SqlSelect[T] {
  def names: List[String]
  def name: String
  def select: String
  def selectWhere: String
  def selectWhere(ob:T): String
  def bindValuesOb(ob: T): Seq[Any]
  def getClassesFieldType: List[String]
  def fromResultSet(rs: java.sql.ResultSet): T
  def bindValues(a: PrimaryKeyFields[T]#Out): Seq[Any]
}



object SqlSelect:
  private inline def summonAllUnwrapped[Elems <: Tuple]: List[Rs[?]] =
    inline erasedValue[Elems] match
      case _: EmptyTuple => Nil
      case _: (h *: t) =>
        summonInline[Rs[h]] :: summonAllUnwrapped[t]
  transparent inline given derived[T <: Product](using m: Mirror.ProductOf[T]): SqlSelect[T] = {
    new SqlSelect[T] {
      def name: String = constValue[m.MirroredLabel]

      def names: List[String] =
        constValueTuple[m.MirroredElemLabels].productIterator.toList.map(_.toString)

      def select: String =   summonInline[SelectStatement[T]].select



      def fromResultSet(rs: java.sql.ResultSet):T = {
        val labels = constValueTuple[m.MirroredElemLabels].productIterator.toList.map(_.toString)
        val readers = summonAllUnwrapped[m.MirroredElemTypes]
        val zipped = labels.zip(getClassesFieldType)
        //    reader.read(rs, label) returns the proper A or F[A] that Rs[_] knows how to do
        val extracted: List[Any] =
          labels.zip(readers).map { case (label, reader) =>
            // we have to widen to Any at runtime,
            // but type safety was enforced at compile time
            reader.asInstanceOf[Rs[Any]].read(rs, label)
          }

        // 4) pack into a Tuple and let the Mirror build your case class
        val tupled = Tuple.fromArray(extracted.toArray)
        m.fromProduct(tupled)
      }
      def getClassesFieldType: List[String] = {
        inline m match {
          case m: Mirror.ProductOf[T] => {

            summonFieldTypes[m.MirroredElemTypes]
          }

        }
      }

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

        val sql = s"SELECT ${fields.mkString(",")} from $tableName WHERE $primaryKey ;"
        sql

      }

      def bindValuesOb(a: T): Seq[Any] = {
        val fields = constValueTuple[m.MirroredElemLabels].toList.map(_.toString)

        val values = a.productIterator.toSeq

        (values) collect {
          ///case None => null // Handle Option[None] correctly
          case Some(v) => v // Extract value from Option[T]
          case other if other != None => other // Use raw value for primitives, excluding nulls
        }

      }
      def bindValues(a: PrimaryKeyFields[T]#Out): Seq[Any] = {
        val fields = constValueTuple[m.MirroredElemLabels].toList.map(_.toString)

        val primaryKeyValue = a.productIterator.toSeq

        (primaryKeyValue) collect {
          //case None => null // Handle Option[None] correctly
          case Some(v) => v // Extract value from Option[T]
          case other if other != None => other // Use raw value for primitives, excluding nulls
        }

      }

    }
  }

extension [T<:Product](entity: T)(using sql: SqlSelect[T])
  def fields: List[String] = summon[SqlSelect[T]].names
  def select: String = summon[SqlSelect[T]].select
  def classFieldTypes: List[String] = summon[SqlSelect[T]].getClassesFieldType

