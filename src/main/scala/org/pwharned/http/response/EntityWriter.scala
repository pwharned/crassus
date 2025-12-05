
package org.pwharned.http.response

import java.nio.ByteBuffer
import java.nio.channels.SocketChannel
import java.nio.charset.StandardCharsets

import org.pwharned.http.codec.Codec
import org.pwharned.http.response.HttpResponse

trait EntityWriter[E]:

  // Writes the *entire* HTTP response, not just the entity.
  final def writeResponse(
                           response: HttpResponse[E],
                           buffer: ByteBuffer,
                           channel: SocketChannel
                         ): Unit =
    buffer.clear()

    // -----------------------------
    // Status line
    // -----------------------------
    putAscii(buffer, "HTTP/1.1 ")
    putAscii(buffer, response.status.toString)
    putAscii(buffer, " ")
    putAscii(buffer, statusText(response.status))
    putAscii(buffer, "\r\n")

    // -----------------------------
    // Headers
    // -----------------------------
    // merge user headers + entity headers
    val allHeaders = response.headers ++ contentHeaders(response.entity)

    allHeaders.foreach { (k, v) =>
      putAscii(buffer, k)
      putAscii(buffer, ": ")
      putAscii(buffer, v)
      putAscii(buffer, "\r\n")
    }

    // end of headers
    putAscii(buffer, "\r\n")

    // -----------------------------
    // Body / streaming strategy
    // -----------------------------
    write(response.entity, buffer, channel)

    // final flush if anything left
    buffer.flip()
    while buffer.hasRemaining do
      channel.write(buffer)


  // Implemented by specific writers (strict, chunked, SSE, etc.)
  def write(entity: E, buffer: ByteBuffer, channel: SocketChannel): Unit

  // Extra headers needed by this writer
  def contentHeaders(entity: E): Seq[(String, String)]


  // --------------------------------------------
  // Helpers (lifted unchanged from your old code)
  // --------------------------------------------
  protected def putAscii(buf: ByteBuffer, s: String): Unit =
    var i = 0
    while i < s.length do
      buf.put(s.charAt(i).toByte)
      i += 1

  protected def statusText(code: Int): String = code match
    case 200 => "OK"
    case 201 => "Created"
    case 204 => "No Content"
    case 400 => "Bad Request"
    case 401 => "Unauthorized"
    case 403 => "Forbidden"
    case 404 => "Not Found"
    case 500 => "Internal Server Error"
    case _ => "Unknown"

object EntityWriter:
  given stringWriter: EntityWriter[String] with
  
    def write(entity: String, buffer: ByteBuffer, channel: SocketChannel): Unit =
      val bytes = entity.getBytes(StandardCharsets.UTF_8)
  
      if bytes.length <= buffer.remaining() then
        buffer.put(bytes)
      else
        var off = 0
        while off < bytes.length do
          val chunk = Math.min(buffer.remaining(), bytes.length - off)
          buffer.put(bytes, off, chunk)
          buffer.flip()
          while buffer.hasRemaining do channel.write(buffer)
          buffer.clear()
          off += chunk
  
    def contentHeaders(entity: String): Seq[(String, String)] =
      val len = entity.getBytes(StandardCharsets.UTF_8).length
      Seq(
        "Content-Type"   -> "text/plain; charset=utf-8",
        "Content-Length" -> len.toString
      )
