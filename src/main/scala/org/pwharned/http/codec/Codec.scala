package org.pwharned.http.codec

import org.pwharned.http.HttpTypes
import org.pwharned.http.HttpTypes.ByteSlice
import org.pwharned.json.{JsonDeserializer, JsonSerializer}

import java.nio.charset.StandardCharsets
import scala.deriving.Mirror
import scala.util.{Try, Right as slice}

trait Codec[A]:
  def decode(slice: HttpTypes.ByteSlice): Either[String, A]
  def encode(value: A): String
  def contentType: String

object Codec:
  // Unit codec for requests without meaningful bodies
  given unitCodec: Codec[Unit] with
    def decode(slice: HttpTypes.ByteSlice): Either[String, Unit] = Right(())
    def encode(value: Unit): String = ""
    def contentType: String = "text/plain"

  // String codec for simple text
  given stringCodec: Codec[String] with
    def decode(slice: HttpTypes.ByteSlice): Either[String, String] =
      Right(slice.toString)
    def encode(value: String): String = value
    def contentType: String = "text/plain; charset=utf-8"

  given entityCodec[A <: Product](using
      m: Mirror.ProductOf[A],
      jd: JsonDeserializer[A],
      js: JsonSerializer[A]
  ): Codec[A] = new Codec[A]:
    def decode(slice: ByteSlice): Either[String, A] =
      val bytes = slice.toBytes
      Try {
        jd.decode(bytes, 0)
      }.toEither match {
        case Left(value)  => Left(value.getMessage)
        case Right(value) => Right(value._1)
      }
    def encode(entity: A): String =
      js.serialize(entity)

    def contentType: String = "application/json"
