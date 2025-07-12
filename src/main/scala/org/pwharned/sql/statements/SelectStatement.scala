package org.pwharned.sql.statements

import org.pwharned.sql.dialect.SqlDialect
import org.pwharned.sql.statements.SelectStatement

import scala.compiletime.*
import scala.concurrent.{ExecutionContext, Future}
import scala.deriving.*
import scala.language.implicitConversions

trait SelectStatement[T] {
  def select: String

}

// you could also
object SelectStatement:
  transparent inline given derived[T<: Product] (using m: Mirror.ProductOf[T], s: SqlDialect):SelectStatement[T] =
    new SelectStatement[T] {
      def name: String = constValue[m.MirroredLabel]

      def names: List[String] =
        constValueTuple[m.MirroredElemLabels].productIterator.toList.map(_.toString)

      def select: String = s.select(name, names)
    }


    
