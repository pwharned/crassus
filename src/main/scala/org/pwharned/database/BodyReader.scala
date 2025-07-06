package org.pwharned.http

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import org.pwharned.json.JsonDeserializer

/** A purely compile‐time “how do I read A from the raw ByteBuffer body?” */
trait BodyReader[A]:
  def read(buffer: ByteBuffer): Either[String, A]

object BodyReader:
  /** Any JsonDeserializer[A] can act as a BodyReader[A] */
  given jsonReader[A](using J: JsonDeserializer[A]): BodyReader[A] with
    def read(buffer: ByteBuffer): Either[String, A] =
      val dup = buffer.duplicate()
      val arr = new Array[Byte](dup.remaining())
      dup.get(arr)
      val s = new String(arr, StandardCharsets.UTF_8)
      J.deserialize(s) match {
        case Left(value) => Left(value.message)
        case Right(value) => Right(value._1)
      }

  given byteBufReader: BodyReader[ByteBuffer] with
    def read(b: ByteBuffer) = Right(b)

  given unitReader: BodyReader[Unit] with
    def read(buffer: ByteBuffer): Either[String, Unit] = Right(())
