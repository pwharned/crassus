package org.pwharned.database

import org.pwharned.database.summonFieldTypes
import HKD.*
import org.pwharned.database.statements.SqlDialect

import scala.language.implicitConversions
import scala.compiletime.*
import scala.concurrent.{ExecutionContext, Future}
import scala.deriving.*

trait InsertStatement[T] {
  def insert: String
  def name: String
  def names: List[String]

}

// you could also
object InsertStatement:
  transparent inline given derived[T<: Product] (using m: Mirror.ProductOf[T], s: SqlDialect):InsertStatement[T] =
    new InsertStatement[T] {
      def name: String = constValue[m.MirroredLabel]

      def names: List[String] =
        constValueTuple[m.MirroredElemLabels].productIterator.toList.map(_.toString)

      def insert: String = s.insertReturning(name, names)
    }


    
