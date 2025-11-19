package org.pwharned.http.response

import java.nio.ByteBuffer
import java.nio.channels.SocketChannel

/**
 * HTTP Response - simple data class.
 * Rendering strategy determined by entity type E.
 */
case class HttpResponse[E](
                            status: Int,
                            headers: Seq[(String, String)],
                            entity: E
                          )

object HttpResponse:

  /** Convenience constructors */
  def ok[E](entity: E): HttpResponse[E] =
    HttpResponse(200, Seq.empty, entity)

  def created[E](entity: E): HttpResponse[E] =
    HttpResponse(201, Seq.empty, entity)

  def notFound[E](entity: E): HttpResponse[E] =
    HttpResponse(404, Seq.empty, entity)

  def error[E](entity: E): HttpResponse[E] =
    HttpResponse(500, Seq.empty, entity)

  /**
   * Render response to channel.
   * Uses EntityWriter type class to determine rendering strategy.
   */
  def render[E](
                 buffer: ByteBuffer,
                 channel: SocketChannel,
                 response: HttpResponse[E]
               )(using writer: EntityWriter[E]): Unit =
    buffer.clear()

    // 1. Write status line and headers

    val allHeaders = response.headers ++ writer.contentHeaders(response.entity)

    buffer.putAscii("HTTP/1.1 ")
      .putAscii(response.status.toString)
      .putAscii(" ")
      .putAscii(EntityWriter.statusText(response.status))
      .putAscii("\r\n")

    allHeaders.foreach { (k, v) =>
      buffer.putAscii(k).putAscii(": ").putAscii(v).putAscii("\r\n")
    }

    buffer.putAscii("\r\n")

    // 2. Write entity (strategy determined by type)
    writer.write(response.entity, buffer, channel)

    // 3. Flush any remaining
    if buffer.position() > 0 then
      EntityWriter.flushBuffer(buffer, channel)

  // Make extension available
  export EntityWriter.putAscii
  private def statusText = EntityWriter.statusText
