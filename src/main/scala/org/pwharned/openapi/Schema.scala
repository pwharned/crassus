package org.pwharned.openapi


import org.pwharned.database.summonFieldTypes

import java.nio.ByteBuffer
import scala.language.implicitConversions
import scala.compiletime.*
import scala.concurrent.{ExecutionContext, Future}
import scala.deriving.*

trait Schema[T] {
  def labels: List[String]
  def `type`: Option[String] 
  def format: Option[String]  = None
  def toSchema: schema
}



object Schema:

  private inline def summonAll[Elems <: Tuple]: List[Schema[_]] =
    inline erasedValue[Elems] match
      case _: EmptyTuple => Nil
      case _: (h *: t) => summonInline[Schema[h]] :: summonAll[t]

  transparent inline given derived[T <: Product](using m: Mirror.ProductOf[T]): Schema[T] =
    new Schema[T] {
      // the field names of the case class
      def labels: List[String] =
        constValueTuple[m.MirroredElemLabels].toList.map(_.toString)

      // every case‐class is an object schema
      def `type`: Option[String] = Some("object")

      // build a Map[name -> childSchema]
      def toSchema: schema =
        // 1) get a Schema[_] for each field
        val childSchemas: List[Schema[_]] = summonAll[m.MirroredElemTypes]

        // 2) ask each to produce its `schema`
        val props: Map[String, schema] =
          labels
            .zip(childSchemas)
            .map { case (name, schm) => name -> schm.toSchema }
            .toMap

        // 3) wrap in the outer schema
        schema(
          `type` = Some("object"),
          properties = Some(props)
        )
    }
// you’ll also need base instances for primitives, lists, etc.
given Schema[Int] with
  def labels = Nil
  def `type`  = Some("integer")
  override def format = Some("int32")
  def toSchema = schema(`type` = `type`, format = format)

given Schema[String] with
  def labels = Nil
  def `type`  = Some("string")
  def toSchema = schema(`type` = `type`)

given Schema[Unit] with
  def labels = Nil

  def `type` = None

  def toSchema = schema(`type` = `type`)

given Schema[ByteBuffer] with
  def labels = Nil

  def `type` = None

  def toSchema = schema(`type` = `type`)

given [A](using sch: Schema[A]): Schema[Iterator[A]] with
  def labels = Nil
  def `type`  = Some("array")
  def toSchema = schema(`type` = `type`, items = Some(sch.toSchema))

given [A](using sch: Schema[A]): Schema[Map[String, A]] with
  def labels: List[String]           = Nil
  def `type`: Option[String]         = Some("object")
  override def toSchema: schema =
    schema(
      `type`                 = `type`,
      additionalProperties   = Some(sch.toSchema)
    )

