package org.pwharned.http.response

import org.pwharned.codec.Codec

import java.nio.ByteBuffer
import java.nio.channels.SocketChannel
import java.nio.charset.StandardCharsets

trait EntitySerializer[E] {
  /**
   * Calculates the size of the serialized entity in bytes.
   * This should be as cheap as possible and avoid full serialization if possible.
   *
   * @param e The entity to measure.
   * @return The size in bytes.
   */
  def calculateSize(e: E): Int
  def serialize(e: E): String

  /**
   * Writes the serialized form of entity `e` into the provided `ByteBuffer`.
   * The `ByteBuffer` should be positioned correctly by the caller before this call.
   * This method should *not* clear or flip the buffer.
   *
   * @param e The entity to serialize.
   * @param writeBuf The ByteBuffer to write into.
   */
  def writeEntity(e: E, writeBuf: ByteBuffer): Unit

  /**
   * Provides headers specific to the serialized entity (e.g., Content-Type, Content-Length).
   *
   * @param e The entity.
   * @param serializedSize The size of the serialized entity.
   */
  def headers(serializedSize: Int): Seq[(String, String)]
}

object EntitySerializer {
  inline given jsonEntitySerializer[E: Codec]: EntitySerializer[E] =
    new EntitySerializer[E] {

      // For JSON, we typically need to serialize to string first to get length
      // This is still an allocation, but it's contained.
      // We'll focus on avoiding the *second* byte[] and ByteBuffer allocation.
      private def getJsonBytes(e: E): String = {
        summon[Codec[E]].encode(e)
      }
      def serialize(e: E): String = {
        summon[Codec[E]].encode(e)
      }
      override def calculateSize(e: E): Int = {

        getJsonBytes(e).length
      }

      override def writeEntity(e: E, writeBuf: ByteBuffer): Unit = {
        val bytes = getJsonBytes(e) // This is where the byte[] is allocated, but only once per request.
        writeBuf.put(bytes.getBytes(StandardCharsets.UTF_8))
      }

      override def headers(serializedSize: Int): Seq[(String, String)] = {
        Seq(
          "Content-Type"   -> "application/json",
          "Content-Length" -> serializedSize.toString
        )
      }
    }

  given stringEntitySerializer: EntitySerializer[String] =
    new EntitySerializer[String] {
      private val UTF8 = StandardCharsets.UTF_8

      private def getStringBytes(e: String): Array[Byte] = e.getBytes(UTF8)
      def serialize(e: String): String = {
        summon[Codec[String]].encode(e)
      }
      override def calculateSize(e: String): Int = getStringBytes(e).length

      override def writeEntity(e: String, writeBuf: ByteBuffer): Unit = {
        val bytes = getStringBytes(e)
        writeBuf.put(bytes)
      }

      override def headers(serializedSize: Int): Seq[(String, String)] = {
        Seq(
          "Content-Type"   -> "text/plain",
          "Content-Length" -> serializedSize.toString
        )
      }
    }
}
