package org.pwharned.macros
import scala.deriving.*
import scala.compiletime.*
import generated.PrimaryKey

import scala.util.Try

trait PrimaryKeyFields[T] {
  type Out
}

given [T](using m: Mirror.ProductOf[T]): PrimaryKeyFields[T] with {
  type Out = Tuple.Filter[m.MirroredElemTypes, [X] =>> X match {
    case PrimaryKey[t] => true
  }]
  
}

trait FromString[A]:
  def parse(s: String): Option[A]

object FromString:
  given FromString[Int] with
    def parse(s: String): Option[Int] = Try(s.toInt).toOption

  given FromString[String] with
    def parse(s: String): Option[String] = Some(s)

// Additional instances for other types


import generated.PrimaryKey  // using your existing PrimaryKey

trait KeyTupleBuilder[T]:
  def build(keys: List[String]): Option[T]

object KeyTupleBuilder:
  // Base case: an empty tuple can be constructed only from an empty list.
  given emptyTupleBuilder: KeyTupleBuilder[EmptyTuple] with
    def build(keys: List[String]): Option[EmptyTuple] =
      if keys.isEmpty then Some(EmptyTuple)
      else None

  // Inductive case:
  // If the expected tuple is of the form PrimaryKey[A] *: T, use the head of the list,
  // parse it to an A, wrap it as a PrimaryKey, then recursively build the tail.
  given inductiveBuilder[A, T <: Tuple](using tailBuilder: KeyTupleBuilder[T], fs: FromString[A])
  : KeyTupleBuilder[PrimaryKey[A] *: T] with
    def build(keys: List[String]): Option[PrimaryKey[A] *: T] = keys match
      case head :: tail =>
        fs.parse(head).flatMap { a =>
          tailBuilder.build(tail).map { tailTuple =>
            PrimaryKey(a) *: tailTuple
          }
        }
      case Nil =>
        None



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



