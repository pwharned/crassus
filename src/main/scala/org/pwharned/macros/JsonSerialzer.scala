package org.pwharned.macros

import scala.deriving.Mirror
import scala.deriving.*
import scala.compiletime.*

trait JsonSerializer[T]:
  def serialize(obj: T): String

object JsonSerializer:
  inline given derived[T <: Product](using m: Mirror.ProductOf[T]): JsonSerializer[T] =
    new JsonSerializer[T]:
      def serialize(obj: T): String =
        val fieldNames = constValueTuple[m.MirroredElemLabels].toIArray.toList.map(_.toString)
        val fieldValues = obj.productIterator.toList.map {
          case s: String => s""""$s"""" // Quote strings
          case other => other.toString // Use raw values for numbers, booleans, etc.
        }
        val jsonFields = fieldNames.zip(fieldValues).map { case (k, v) => s""""$k": $v""" }
        s"{ ${jsonFields.mkString(", ")} }"

