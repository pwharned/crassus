package org.pwharned.macros

import scala.deriving.*
import scala.compiletime.*
import scala.concurrent.{ExecutionContext, Future}
import org.pwharned.macros.HKD.*

trait SqlSelect[T] {
  def names: List[String]
  def name: String
  def select: String
  def getClassesFieldType: List[String]
  def fromResultSet(rs: java.sql.ResultSet): T

}



object SqlSelect:
  transparent inline given derived[T <: Product](using m: Mirror.ProductOf[T]): SqlSelect[T] = {
    new SqlSelect[T] {
      def name: String = constValue[m.MirroredLabel]
      def names: List[String] =
        constValueTuple[m.MirroredElemLabels].toIArray.toList.map(_.toString)
      def select: String = s"select ${names.mkString(",") } from ${name}"
      def createTable: String = s"create table if not exists $name( "
      def fromResultSet(rs: java.sql.ResultSet):T = {
        val labels = constValueTuple[m.MirroredElemLabels].toIArray.toList.map(_.toString)
        val zipped = labels.zip(getClassesFieldType)
        val extractedValues = zipped.map {
          case (label, "String") => rs.getString(label)
          case (label, "Option[String]") => Option(rs.getString(label))
          case (label, "Int") => rs.getInt(label)
          case (label, "Boolean") => rs.getBoolean(label)
          case (label, "Double") => rs.getDouble(label)
          case (label, "Float") => rs.getFloat(label)
          case (label, "Long") => rs.getLong(label)
          case (label, "Short") => rs.getShort(label)
          case (label, "Byte") => rs.getByte(label)
          case _ => throw new IllegalArgumentException(s"Unsupported field type")
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

    }
  }

