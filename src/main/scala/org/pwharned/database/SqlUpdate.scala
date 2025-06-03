package org.pwharned.database


import scala.compiletime.*
import scala.deriving.*

trait SqlUpdate[T]:
  def updateStatement(obj: T): String

  def bindValues(obj: T): Seq[Any]
  def bindValues(obj: T, b: PrimaryKeyFields[T]#Out): Seq[Any]

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

      }

      def bindValues(obj: T,a: PrimaryKeyFields[T]#Out): Seq[Any] = {
         val fields = constValueTuple[m.MirroredElemLabels].toList.map(_.toString)

         val primaryKey = PrimaryKeyExtractor.getPrimaryKey[T]
        val  nonPrimaryFields = fields.filter(x => !primaryKey.contains(x)) // Fields used in update
        val fieldValues = fields.zip(obj.productIterator).toMap

        val updateValues = nonPrimaryFields.flatMap(field => fieldValues.get(field))

        val primaryKeyValue = a.productIterator

        (updateValues ++ primaryKeyValue) map {
          case None => null // Handle Option[None] correctly
          case Some(v) => v // Extract value from Option[T]
          case other => other // Use raw value for primitives
        }

      }


extension[T <: Product] (entity: T) (using sql: SqlUpdate[T], sqlDelete: SqlDelete[T] )
  def updateStatement(): String = summon[SqlUpdate[T]].updateStatement(entity)
  def bindValues: Seq[Any] = summon[SqlUpdate[T]].bindValues(entity)


