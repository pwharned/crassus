package org.pwharned.codec

import org.pwharned.http.HttpTypes

import java.nio.charset.StandardCharsets
enum CodecTypes:
  case object Json
trait Codec[A]:
  def decode(slice: HttpTypes.ByteSlice): Either[String, A]
  def encode(value: A): Array[Byte]
  def contentType: String

object Codec:
  // Unit codec for requests without meaningful bodies
  given unitCodec: Codec[Unit] with
    def decode(slice: HttpTypes.ByteSlice): Either[String, Unit] = Right(())
    def encode(value: Unit): Array[Byte] = Array.empty
    def contentType: String = "text/plain"

  // String codec for simple text
  given stringCodec: Codec[String] with
    def decode(slice: HttpTypes.ByteSlice): Either[String, String] =
      Right(slice.toString)
    def encode(value: String): Array[Byte] = value.getBytes(StandardCharsets.UTF_8)
    def contentType: String = "text/plain; charset=utf-8"

  // Byte array codec for raw data
  given bytesCodec: Codec[Array[Byte]] with
    def decode(slice: HttpTypes.ByteSlice): Either[String, Array[Byte]] =
      Right(slice.toBytes)
    def encode(value: Array[Byte]): Array[Byte] = value
    def contentType: String = "application/octet-stream"
