package org.pwharned.macros
import scala.deriving.*
import scala.compiletime.*

trait SqlUpdate[T]:
  def updateStatement(obj: T): String

  def bindValues(obj: T): Seq[Any]

object SqlUpdate:
  inline given derive[T <: Product](using m: Mirror.ProductOf[T]): SqlUpdate[T] =
    new SqlUpdate[T]:
      def updateStatement(obj: T): String =
        val tableName = constValue[m.MirroredLabel]
        val fields = constValueTuple[m.MirroredElemLabels].toList.map(_.toString)

        val primaryKey = PrimaryKeyExtractor.getPrimaryKey[T].map (x=> s" $x = ? ").mkString(" AND ")
        val nonPrimaryFields = fields.filter( x=>  !primaryKey.contains(x) ).map(name => s"$name = ?").mkString(", ")

        s"SELECT * FROM FINAL TABLE(UPDATE $tableName SET $nonPrimaryFields WHERE $primaryKey )"

      def bindValues(obj: T): Seq[Any] = {
        val fields = constValueTuple[m.MirroredElemLabels].toList.map(_.toString)

        val primaryKey = PrimaryKeyExtractor.getPrimaryKey[T]
        val nonPrimaryFields = fields.filter( x=>  !primaryKey.contains(x) ) // Fields used in update
        val fieldValues = fields.zip(obj.productIterator).toMap

        // Bind update values first (exclude primary key)
        val updateValues = nonPrimaryFields.flatMap(field => fieldValues.get(field))

        // Bind primary key value for WHERE clause
        val primaryKeyValue = fieldValues.filter(x => primaryKey.contains(x._1)).values.toSeq

        (updateValues ++ primaryKeyValue) map {
          case None    => null  // Handle Option[None] correctly
          case Some(v) => v     // Extract value from Option[T]
          case other   => other // Use raw value for primitives
        }
        // Concatenating the sequences in the right order

      }


