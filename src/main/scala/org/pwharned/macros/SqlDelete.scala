package org.pwharned.macros
import scala.deriving.*
import scala.compiletime.*
import generated.PrimaryKey

trait PrimaryKeyFields[T] {
  type Out
}

given [T](using m: Mirror.ProductOf[T]): PrimaryKeyFields[T] with {
  type Out = Tuple.Filter[m.MirroredElemTypes, [X] =>> X match {
    case PrimaryKey[t] => true
  }]
  
}





trait SqlDelete[T]:
  def deleteStatement: String
  def bindValues(pkValues: PrimaryKeyFields[T]#Out): Seq[Any] // Extract values separately

object SqlDelete:
  inline given derive[T <: Product](using m: Mirror.ProductOf[T]): SqlDelete[T] =
    new SqlDelete[T]:
      def deleteStatement: String =
        val tableName = constValue[m.MirroredLabel]

        val primaryKey = PrimaryKeyExtractor.getPrimaryKey[T].map( x => s""" $x = ? """).mkString(" AND ")

        s"DELETE FROM $tableName WHERE $primaryKey"

      def bindValues(pkValues: PrimaryKeyFields[T]#Out): Seq[Any] =
        pkValues match {
          case tuple: Tuple => tuple.toList
          case singleValue  => Seq(singleValue) // Handles cases where there's only one primary key
        }



