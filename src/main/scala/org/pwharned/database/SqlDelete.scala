package org.pwharned.database

import generated.{ user}
import org.pwharned.database.HKD._
import org.pwharned.macros.{listToTuple}

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
  inline given derive[T <: Product](using m: Mirror.ProductOf[T]): SqlDelete[T] =
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



