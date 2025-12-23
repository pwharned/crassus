package org.pwharned.openapi

import org.pwharned.database.hkd._

import java.nio.ByteBuffer
import scala.compiletime.*
import scala.concurrent.{ExecutionContext, Future}
import scala.deriving.*
import scala.language.implicitConversions

trait Schema[T] {
  def labels: List[String]
  def `type`: Option[String]
  def format: Option[String] = None
  def toSchema: schema
}

object Schema:

  private inline def summonAll[Elems <: Tuple]: List[Schema[?]] =
    inline erasedValue[Elems] match
      case _: EmptyTuple => Nil
      case _: (h *: t)   => summonInline[Schema[h]] :: summonAll[t]

  transparent inline given derived[T <: Product](using
      m: Mirror.ProductOf[T]
  ): Schema[T] =
    new Schema[T] {
      // the field names of the case class
      def labels: List[String] =
        constValueTuple[m.MirroredElemLabels].toList.map(_.toString)

      // every case‐class is an object schema
      def `type`: Option[String] = Some("object")

      // build a Map[name -> childSchema]
      def toSchema: schema =
        // 1) get a Schema[_] for each field
        val childSchemas: List[Schema[?]] = summonAll[m.MirroredElemTypes]

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

  given [A](using sch: Schema[A]): Schema[PersistedField[A]] with
    def labels: List[String] = Nil

    def `type`: Option[String] = sch.`type`

    override def format: Option[String] = sch.format

    def toSchema: schema = sch.toSchema

  // 2) Give a Schema for Default[A] (if you have a Default wrapper)
  given [A](using sch: Schema[A]): Schema[Default[A]] with
    def labels: List[String] = Nil

    def `type`: Option[String] = sch.`type`

    override def format: Option[String] = sch.format

    def toSchema: schema = sch.toSchema
  given Schema[Boolean] with
    def labels = Nil

    def `type` = Some("boolean")

    def toSchema = schema(`type` = `type`)
  given Schema[Float] with
    def labels = Nil

    def `type` = Some("number")

    def toSchema = schema(`type` = `type`, format = Some("float"))
  given Schema[Int] with
    def labels = Nil
    def `type` = Some("integer")
    override def format = Some("int32")
    def toSchema = schema(`type` = `type`, format = format)

  given Schema[String] with
    def labels = Nil
    def `type` = Some("string")
    def toSchema = schema(`type` = `type`)

  given Schema[java.time.Instant] with
    def labels = Nil

    def `type` = Some("string")

    def toSchema = schema(`type` = `type`)

  given Schema[Unit] with
    def labels = Nil

    def `type` = None

    def toSchema = schema(`type` = `type`)

  given Schema[ByteBuffer] with
    def labels = Nil

    def `type` = None

    def toSchema = schema(`type` = `type`)

  given Schema[java.util.UUID] with
    def labels = Nil

    def `type` = Some("string")

    def toSchema = schema(`type` = `type`, format = Some("UUID4"))

  given [A](using sch: Schema[A]): Schema[Iterator[A]] with
    def labels = Nil
    def `type` = Some("array")
    def toSchema = schema(`type` = `type`, items = Some(sch.toSchema))

  given [A](using sch: Schema[A]): Schema[Vector[A]] with
    def labels = Nil

    def `type` = Some("array")

    def toSchema = schema(`type` = `type`, items = Some(sch.toSchema))

  given [A](using sch: Schema[A]): Schema[List[A]] with
    def labels = Nil

    def `type` = Some("array")

    def toSchema = schema(`type` = `type`, items = Some(sch.toSchema))

  given [A](using sch: Schema[A]): Schema[PrimaryKey[A]] with
    def labels = Nil

    def `type` = sch.`type`

    def toSchema = schema(`type` = `type`, items = Some(sch.toSchema))

  given [A](using sch: Schema[A]): Schema[GeneratedPrimaryKey[A]] with
    def labels = Nil

    def `type` = sch.`type`

    def toSchema = schema(`type` = `type`, items = Some(sch.toSchema))
  given [A](using sch: Schema[A]): Schema[Option[A]] with
    def labels = Nil

    def `type` = sch.`type`

    def toSchema = schema(`type` = `type`, items = Some(sch.toSchema))

  given [A](using sch: Schema[A]): Schema[Nullable[A]] with
    def labels = Nil

    def `type` = sch.`type`

    def toSchema = schema(`type` = `type`, items = Some(sch.toSchema))

  given [A](using sch: Schema[A]): Schema[Map[String, A]] with
    def labels: List[String] = Nil
    def `type`: Option[String] = Some("object")
    override def toSchema: schema =
      schema(
        `type` = `type`,
        additionalProperties = Some(sch.toSchema)
      )
