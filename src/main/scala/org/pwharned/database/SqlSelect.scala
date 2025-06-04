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
  def getClassesFieldType: List[String]
  def fromResultSet(rs: java.sql.ResultSet): T
  def bindValues(a: PrimaryKeyFields[T]#Out): Seq[Any]
}



object SqlSelect:
  transparent inline given derived[T <: Product](using m: Mirror.ProductOf[T]): SqlSelect[T] = {
    new SqlSelect[T] {
      def name: String = constValue[m.MirroredLabel]

      def names: List[String] =
        constValueTuple[m.MirroredElemLabels].productIterator.toList.map(_.toString)

      def select: String = {
        s"select ${names.mkString(",") } from ${name}"
      }
      def fromResultSet(rs: java.sql.ResultSet):T = {
        val labels = constValueTuple[m.MirroredElemLabels].productIterator.toList.map(_.toString)
        val zipped = labels.zip(getClassesFieldType)
        val extractedValues = zipped.map {
          case (label, "String") => rs.getString(label)
          case (label, "Option[String]") => Option(rs.getString(label))
          case (label, "Int") => rs.getInt(label)
          case (label, "Option[Int]") => Option(rs.getInt(label))
          case (label, "Option[Boolean]") => Option(rs.getBoolean(label))
          case (label, "Boolean") => rs.getBoolean(label)
          case (label, "Option[Double]") => Option(rs.getDouble(label))
          case (label, "Double") => rs.getDouble(label)
          case (label, "Float") => rs.getFloat(label)
          case (label, "Option[Float]") => Option(rs.getFloat(label))
          case (label, "Option[Long]") => Option(rs.getLong(label))
          case (label, "Long") => rs.getLong(label)
          case (label, "Option[Short]") => Option(rs.getShort(label))
          case (label, "Short") => rs.getShort(label)
          case (label, "Option[Byte]") => Option(rs.getByte(label))
          case (label, "Byte") => rs.getByte(label)
          case (label, x: String) => throw new IllegalArgumentException(s"Unsupported field type $x")
        }

        // Convert to Tuple for Mirror's apply method
        val valuesTuple = Tuple.fromArray(extractedValues.toArray)

        // Use Mirror to instantiate case class
        m.fromProduct(valuesTuple)
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


        val sql = s"SELECT ${fields.mkString(",")} from $tableName where $where  "
        sql
        
      def selectWhere: String = {
        val tableName = constValue[m.MirroredLabel]
        val fields = constValueTuple[m.MirroredElemLabels].toList.map(_.toString)
     
        val primaryKey = PrimaryKeyExtractor.getPrimaryKey[T].map(x => s" $x = ? ").mkString(" AND ")

        val sql = s"SELECT ${fields.mkString(",")} from $tableName WHERE $primaryKey "
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

