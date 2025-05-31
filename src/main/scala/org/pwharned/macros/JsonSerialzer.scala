package org.pwharned.macros

import scala.deriving.Mirror
import scala.deriving.*
import scala.compiletime.*

trait JsonSerializer[T]:
  def serialize(obj: T): String
  def serialize(obj: List[T]): String
  def serialize(obj: Iterator[T]): String

object JsonSerializer:
  inline given derived[T <: Product](using m: Mirror.ProductOf[T]): JsonSerializer[T] =
    new JsonSerializer[T]:
      def serialize(obj: T): String =
        val fieldNames = constValueTuple[m.MirroredElemLabels].toIArray.toList.map(_.toString)
        val productValues = obj.asInstanceOf[Product].productIterator.toList
        val jsonFields = fieldNames.zip(productValues).map { case (k, v) => s""""$k":${serializeValue(v)}""" }
        s"{ ${jsonFields.mkString(", ")} }"
      def serialize(obj:List[T]): String = "[" + obj.map(x => serialize(x)).mkString(",") + "]"

      def serialize(obj: Iterator[T]): String =  obj.foldLeft("[")( (acc, x) =>  acc + serialize( x) + "," ).stripSuffix(",") + "]"


  private def serializeValue(value: Any): String = value match
    case null => "null"
    case s: String => s""""${s.replaceAll("\"", "\\\\\"")}""""  // Escape quotes in strings
    case s: (Int | Long |  Float | Double |  Short |  Byte) => s.toString
    case b: Boolean => b.toString
    case opt: Option[_] => opt.map(serializeValue).getOrElse("null")
    case seq: Seq[_] => seq.map(serializeValue).mkString("[", "", "]")
    case map: Map[_, _] => map.map { case (k, v) => s"${serializeValue(k)}: ${serializeValue(v)}" }.mkString("{", ",", "}")
    case product: Product =>
      val fields = (0 until product.productArity).map(i =>
        serializeValue(product.productElement(i))
      )
      fields.mkString("{", ",", "}")
    case _ => s""""${value.toString}""""
