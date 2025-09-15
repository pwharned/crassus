package org.pwharned.http

import org.pwharned.codec.Codec
import org.pwharned.http.HttpTypes.HeaderName
import org.pwharned.io.IO

import java.nio.charset.StandardCharsets

case class ResponseData[A](
                                    status: StatusCode,
                                    headers: Map[HeaderName, String],
                                    bodyValue: A
                                  )

// Zero-cost opaque HTTP response with full type safety
opaque type HttpResponse[A] = ResponseData[A]

object HttpResponse:
  // Constructor - only way to create HttpResponse
  private[HttpResponse] def apply[A](data: ResponseData[A]): HttpResponse[A] = data

  // Convenience constructors
  def ok[A](body: A)(using Codec[A]): HttpResponse[A] =
    HttpResponse(ResponseData(StatusCode.Ok, Map.empty, body))
  def json[A<:Product](body: A)(using Codec[A]): HttpResponse[A] =
    HttpResponse(ResponseData(StatusCode.Ok, Map.empty, body))

  def created[A](body: A)(using Codec[A]): HttpResponse[A] =
    HttpResponse(ResponseData(StatusCode.Created, Map.empty, body))

  def badRequest[A](body: A)(using Codec[A]): HttpResponse[A] =
    HttpResponse(ResponseData(StatusCode.BadRequest, Map.empty, body))

  def notFound[A](body: A)(using Codec[A]): HttpResponse[A] =
    HttpResponse(ResponseData(StatusCode.NotFound, Map.empty, body))

  def internalError[A](body: A)(using Codec[A]): HttpResponse[A] =
    HttpResponse(ResponseData(StatusCode.InternalServerError, Map.empty, body))

  // Extension methods for zero-cost operations
  extension [A](response: HttpResponse[A])
    // Direct access to underlying data - zero cost
    private inline def data: HttpResponse[A] = response

    // Zero-cost property access
    inline def status: StatusCode = data.status
    inline def headers: Map[HeaderName, String] = data.headers
    inline def body: A = data.bodyValue

    // Zero-cost transformations
    def withStatus(newStatus: StatusCode): HttpResponse[A] =
      HttpResponse(data.copy(status = newStatus))

    def withHeader(name: HeaderName, value: String): HttpResponse[A] =
      HttpResponse(data.copy(headers = data.headers + (name -> value)))

    def as[B](newBody: B): HttpResponse[B] =
      HttpResponse(ResponseData(data.status, data.headers, newBody))

    // Efficient serialization
    def toBytes(using codec: Codec[A]): Array[Byte] =
      val bodyBytes = codec.encode(data.bodyValue)
      val allHeaders = data.headers +
        (HeaderName.ContentType -> codec.contentType) +
        (HeaderName.ContentLength -> bodyBytes.length.toString)

      val headerString = allHeaders.map { case (k, v) => s"${k.value}: $v" }.mkString("\r\n")
      val responseStr = s"HTTP/1.1 ${data.status.code} ${data.status.reasonPhrase}\r\n$headerString\r\n\r\n"
      responseStr.getBytes(StandardCharsets.UTF_8) ++ bodyBytes

    def serverSentEvents(stream: org.pwharned.stream.Stream[IO[A]])(using codec: Codec[A]): HttpResponse[org.pwharned.stream.Stream[IO[String]]] = {
      val sseStream = stream.map(_.map { value =>
        val encoded = new String(codec.encode(value))
        s"data: $encoded\n\n"
      })

      HttpResponse(ResponseData(
        StatusCode.Ok,
        Map(
          HeaderName.ContentType -> "text/event-stream",
          HeaderName.CacheControl -> "no-cache",
          HeaderName.Connection -> "keep-alive"
        ),
        sseStream
      ))
    }

    // Create chunked streaming response
    def chunked(stream: Stream[IO[A]])(using codec: Codec[A]): HttpResponse[Stream[IO[String]]] = {
      val chunkedStream = stream.map(_.map { value =>
        val encoded = codec.encode(value)
        val size = encoded.length.toHexString
        s"$size\r\n${new String(encoded)}\r\n"
      })

      HttpResponse(ResponseData(
        StatusCode.Ok,
        Map(
          HeaderName.ContentType -> "application/json", // or appropriate type
          HeaderName.TransferEncoding -> "chunked"
        ),
        chunkedStream
      ))
    }