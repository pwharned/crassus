package org.pwharned.database.derive


import org.pwharned.database.SqlDialect

import scala.compiletime.*
import scala.deriving.*
import scala.language.implicitConversions

trait SelectStatement[T] {
  def select(): String

}

// you could also
object SelectStatement:
  inline given derived[T<: Product] (using m: Mirror.ProductOf[T], s: SqlDialect):SelectStatement[T] =
    val name: String = constValue[m.MirroredLabel]

    val columnNames = constValueTuple[m.MirroredElemLabels].productIterator.toList.map(_.toString)
    val statement  = s.select(name,columnNames)
    lazy val self: SelectStatement[T] =
      () => statement
    self

    
