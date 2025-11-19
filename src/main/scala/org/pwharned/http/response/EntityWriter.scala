package org.pwharned.http.response

import org.pwharned.http.codec.Codec

import java.nio.ByteBuffer
import java.nio.channels.SocketChannel
import java.nio.charset.StandardCharsets

/**
 * Type class for writing entities to HTTP responses.
 * Entity type determines rendering strategy at compile time.
 */
trait EntityWriter[E]:
  /** Write entity to channel using provided buffer */
  def write(entity: E, buffer: ByteBuffer, channel: SocketChannel): Unit

  /** Additional headers needed for this entity type */
  def contentHeaders(entity: E): Seq[(String, String)]

object EntityWriter:

  // ============================================================================
  // HELPER: Zero-allocation ASCII writer
  // ============================================================================

  extension (buf: ByteBuffer)
    inline def putAscii(s: String): ByteBuffer =
      var i = 0
      while i < s.length do
        buf.put(s.charAt(i).toByte)
        i += 1
      buf

  private inline def writeHeaders(
                                   status: Int,
                                   headers: Seq[(String, String)],
                                   buffer: ByteBuffer,
                                   channel: SocketChannel
                                 ): Unit =
    buffer.putAscii("HTTP/1.1 ")
      .putAscii(status.toString)
      .putAscii(" ")
      .putAscii(statusText(status))
      .putAscii("\r\n")

    headers.foreach { (k, v) =>
      buffer.putAscii(k).putAscii(": ").putAscii(v).putAscii("\r\n")
    }

    buffer.putAscii("\r\n")

  inline def flushBuffer(buffer: ByteBuffer, channel: SocketChannel): Unit =
    buffer.flip()
    while buffer.hasRemaining do
      channel.write(buffer)

  def statusText(code: Int): String = code match
    case 200 => "OK"
    case 201 => "Created"
    case 204 => "No Content"
    case 400 => "Bad Request"
    case 401 => "Unauthorized"
    case 403 => "Forbidden"
    case 404 => "Not Found"
    case 500 => "Internal Server Error"
    case _ => "Unknown"

  // ============================================================================
  // STRICT RESPONSES - Simple entities serialized once
  // ============================================================================

  /** Writer for String entities */
  given stringWriter: EntityWriter[String] with
    def write(entity: String, buffer: ByteBuffer, channel: SocketChannel): Unit =
      val bytes = entity.getBytes(StandardCharsets.UTF_8)

      // If fits in buffer, write all at once
      if bytes.length <= buffer.remaining() then
        buffer.put(bytes)
      else
        // Write in chunks
        var offset = 0
        while offset < bytes.length do
          val remaining = buffer.remaining()
          val chunkSize = Math.min(remaining, bytes.length - offset)
          buffer.put(bytes, offset, chunkSize)

          flushBuffer(buffer, channel)
          buffer.clear()
          offset += chunkSize

    def contentHeaders(entity: String): Seq[(String, String)] =
      val length = entity.getBytes(StandardCharsets.UTF_8).length
      Seq(
        "Content-Type" -> "text/plain; charset=utf-8",
        "Content-Length" -> length.toString
      )

  /** Writer for any entity with a Codec (JSON, XML, etc.) */
  given codecWriter[E](using codec: Codec[E]): EntityWriter[E] with
    def write(entity: E, buffer: ByteBuffer, channel: SocketChannel): Unit =
      val serialized = codec.encode(entity)
      val bytes = serialized.getBytes(StandardCharsets.UTF_8)

      if bytes.length <= buffer.remaining() then
        buffer.put(bytes)
      else
        var offset = 0
        while offset < bytes.length do
          val chunkSize = Math.min(buffer.remaining(), bytes.length - offset)
          buffer.put(bytes, offset, chunkSize)

          flushBuffer(buffer, channel)
          buffer.clear()
          offset += chunkSize

    def contentHeaders(entity: E): Seq[(String, String)] =
      val serialized = codec.encode(entity)
      val length = serialized.getBytes(StandardCharsets.UTF_8).length
      Seq(
        "Content-Type" -> codec.contentType,
        "Content-Length" -> length.toString
      )

  // ============================================================================
  // STREAMING RESPONSES - Special wrapper types
  // ============================================================================

  /** Wrapper for chunked transfer encoding */
  case class Chunked[T](chunks: Iterator[T])(using codec: Codec[T])

  given chunkedWriter[T](using codec: Codec[T]): EntityWriter[Chunked[T]] with
    def write(entity: Chunked[T], buffer: ByteBuffer, channel: SocketChannel): Unit =
      // Headers already written, now stream chunks
      entity.chunks.foreach { chunk =>
        val chunkData = codec.encode(chunk).getBytes(StandardCharsets.UTF_8)
        val sizeHex = f"${chunkData.length}%x"

        // Write chunk size
        buffer.clear()
        buffer.putAscii(sizeHex).putAscii("\r\n")
        flushBuffer(buffer, channel)

        // Write chunk data
        channel.write(ByteBuffer.wrap(chunkData))

        // Write trailing CRLF
        buffer.clear()
        buffer.putAscii("\r\n")
        flushBuffer(buffer, channel)
      }

      // Final chunk
      buffer.clear()
      buffer.putAscii("0\r\n\r\n")
      flushBuffer(buffer, channel)

    def contentHeaders(entity: Chunked[T]): Seq[(String, String)] =
      Seq(
        "Transfer-Encoding" -> "chunked",
        "Content-Type" -> codec.contentType
      )

  /** Wrapper for Server-Sent Events */
  case class SSE[T](events: Iterator[T])(using codec: Codec[T])

  given sseWriter[T](using codec: Codec[T]): EntityWriter[SSE[T]] with
    def write(entity: SSE[T], buffer: ByteBuffer, channel: SocketChannel): Unit =
      // Stream events in SSE format
      entity.events.foreach { event =>
        val eventData = codec.encode(event)

        buffer.clear()
        buffer.putAscii("data: ")
          .putAscii(eventData)
          .putAscii("\n\n")
        flushBuffer(buffer, channel)
      }

    def contentHeaders(entity: SSE[T]): Seq[(String, String)] =
      Seq(
        "Content-Type" -> "text/event-stream",
        "Cache-Control" -> "no-cache",
        "Connection" -> "keep-alive"
      )

  /** Wrapper for byte streams (files, large payloads) */
  case class ByteStream(chunks: Iterator[Array[Byte]])

  given byteStreamWriter: EntityWriter[ByteStream] with
    def write(entity: ByteStream, buffer: ByteBuffer, channel: SocketChannel): Unit =
      entity.chunks.foreach { bytes =>
        channel.write(ByteBuffer.wrap(bytes))
      }

    def contentHeaders(entity: ByteStream): Seq[(String, String)] =
      Seq("Transfer-Encoding" -> "chunked")
