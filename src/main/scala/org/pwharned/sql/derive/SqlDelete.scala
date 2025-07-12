package org.pwharned.sql.derive

import org.pwharned.sql.database.HKD.*

import scala.compiletime.*
import scala.deriving.*
import scala.util.Try

trait PrimaryKeyFields[T] {
  type Out <: Tuple
}


given [T](using m: Mirror.ProductOf[T]): PrimaryKeyFields[T] with {
  type Out = Tuple.Filter[m.MirroredElemTypes, [X] =>> X match {
    case PrimaryKey[t] => true
    case _ => false
  }]
  
}

trait PrimaryKeyFieldLength[T] {
  type Out
}


given [T](using m: Mirror.ProductOf[T]): PrimaryKeyFieldLength[T] with {
  type Out = Tuple.Size[m.MirroredElemTypes ]

}



// Additional instances for other types





trait SqlDelete[T<:Product]:
  def deleteStatement: String
  def bindValues(pkValues: PrimaryKeyFields[T]#Out): Seq[Any] // Extract values separately
  def values(l: List[String]): PrimaryKeyFields[T]#Out
  
object SqlDelete:
  inline def listToTuple[A, T <: Tuple](list: List[A]): T = {
    inline erasedValue[T] match
      case _: EmptyTuple =>
        EmptyTuple.asInstanceOf[T]
      case _: (h *: t) =>
        // Convert the head string to type h

        // Recursively convert the remainder of the list to type t
        val tail: t = listToTuple[Any, t](list.tail)
        (list.head *: tail).asInstanceOf[T]
  }
  transparent inline given derived[T <: Product](using m: Mirror.ProductOf[T]): SqlDelete[T] =
    new SqlDelete[T]:
      def values(l:List[String]):PrimaryKeyFields[T]#Out = listToTuple(l).asInstanceOf[PrimaryKeyFields[T]#Out]
      def deleteStatement: String =
        val tableName = constValue[m.MirroredLabel]

        val primaryKey = PrimaryKeyExtractor.getPrimaryKey[T].map( x => s""" $x = ? """).mkString(" AND ")

        s"DELETE FROM $tableName WHERE $primaryKey"

      def bindValues(pkValues: PrimaryKeyFields[T]#Out): Seq[Any] =
        pkValues match {
          case tuple: Tuple => tuple.toList
          case singleValue  => Seq(singleValue) // Handles cases where there's only one primary key
        }



