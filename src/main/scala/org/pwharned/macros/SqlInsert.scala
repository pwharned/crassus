package org.pwharned.macros
import scala.deriving.*
import scala.compiletime.*

trait SqlInsert[T]:
  def insertStatement: String
  def bindValues(obj: T): Seq[Any] // Extract values separately

object SqlInsert:
  inline given derived[T <: Product](using m: Mirror.ProductOf[T]): SqlInsert[T] =
    new SqlInsert[T]:
      def insertStatement: String =
        val tableName = constValue[m.MirroredLabel]
        val fieldNames = constValueTuple[m.MirroredElemLabels].toIArray.toList.map(_.toString)
        val placeholders = fieldNames.map(_ => "?").mkString(", ")
        s"INSERT INTO $tableName (${fieldNames.mkString(", ")}) VALUES ($placeholders);"

      def bindValues(obj: T): Seq[Any] =
        obj.productIterator.toList.map {
          case None    => null  // Handle Option[None] correctly
          case Some(v) => v     // Extract value from Option[T]
          case other   => other // Use raw value for primitives
        }
