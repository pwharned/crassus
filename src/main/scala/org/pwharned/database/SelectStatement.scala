package org.pwharned.database

import org.pwharned.database.summonFieldTypes
import HKD.*
import scala.language.implicitConversions
import scala.compiletime.*
import scala.concurrent.{ExecutionContext, Future}
import scala.deriving.*

trait SelectStatement[T] {
  def select: String

}

// you could also
object SelectStatement:
  transparent inline given derived[T<: Product] (using m: Mirror.ProductOf[T]):SelectStatement[T] =
    new SelectStatement[T] {
      def name: String = constValue[m.MirroredLabel]

      def names: List[String] =
        constValueTuple[m.MirroredElemLabels].productIterator.toList.map(_.toString)

      def select: String = s"select ${names.mkString(",")} from ${name};"      
    }


    
