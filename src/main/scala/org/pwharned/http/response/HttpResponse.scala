package org.pwharned.http.response

import java.nio.ByteBuffer
import java.nio.channels.SocketChannel

// Pure data - no behavior
case class HttpResponse[E](
                            status: Int,
                            headers: Seq[(String, String)],
                            entity: E
                          )

object HttpResponse:
  // Convenience constructors
  def ok[E](entity: E): HttpResponse[E] =
    HttpResponse(200, Seq.empty, entity)

  def created[E](entity: E): HttpResponse[E] =
    HttpResponse(201, Seq.empty, entity)

  def notFound[E](entity: E): HttpResponse[E] =
    HttpResponse(404, Seq.empty, entity)

  def error[E](entity: E): HttpResponse[E] =
    HttpResponse(500, Seq.empty, entity)

