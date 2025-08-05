package org.pwharned.http

import org.pwharned.http.Headers.Headers
import org.pwharned.http.HttpMethod
import org.pwharned.http.HttpMethod.HttpMethod
import org.pwharned.http.HttpPath.HttpPath

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import scala.compiletime.summonInline

object HttpRequest:
  // The HttpRequest opaque type is now a tuple of four ByteBuffers:
  // (method, path, headers, body)
  opaque type HttpRequest[B] = (ByteBuffer, ByteBuffer, ByteBuffer, B)

  type ReqParam[Req] = Req match
    case Unit => HttpRequest // the old opaque-tuple type
    case _ => HttpRequest[Req] // the new generic version
  extension (req: HttpRequest[ByteBuffer])
    inline def as[B](using br: BodyReader[B]): Either[String, HttpRequest[B]] =
      br.read(req.body).map { b =>
        HttpRequest(req._1, req._2, req._3, b)
      }
  object HttpRequest:
    def apply[B](
                  method: HttpMethod,
                  path: HttpPath,
                  headers: Headers,
                  body: B
                ): HttpRequest[B] =
      (ByteBuffer.wrap(method.getBytes), ByteBuffer.wrap(path.getBytes), ByteBuffer.wrap(headers.getBytes), body)
    // Construct an HttpRequest from its parts.
    def apply[B](
               method: ByteBuffer,
               path: ByteBuffer,
               headers: ByteBuffer,
               body: B
             ): HttpRequest[B] =
      (method, path, headers, body)

    given routeConversion[B, A](using br: BodyReader[B])
    : Conversion[HttpRequest[B] => HttpResponse[A],
      HttpRequest[ByteBuffer] => HttpResponse[A]] =
      handler =>
        rawReq =>
          br.read(rawReq._4) match
            case Left(err) =>
              HttpResponse.error(
                s"Bad Request – cannot parse JSON: $err"
              )
            case Right(decoded) =>
              // re‐package and call the user’s handler
              val typedReq =
                HttpRequest(rawReq._1, rawReq._2, rawReq._3, decoded)
              handler(typedReq)
    def fromFullBuffer(buffer: ByteBuffer): Option[HttpRequest[ByteBuffer]] =
      // Work on a duplicate so we don't modify the caller's buffer.


      def readUntil(delim: Byte): Option[ByteBuffer] =
        val start = buffer.position()
        var found = false
        while buffer.hasRemaining && !found do
          if buffer.get() == delim then found = true
        if !found then None
        else
          val end = buffer.position() - 1
          // Reset position to start and create a slice.
          buffer.position(start)
          val slice = buffer.slice()
          slice.limit(end - start)
          // Advance the position past the delimiter.
          buffer.position(end + 1)
          Some(slice)

      // 1. Extract the METHOD (read until first space).
      val maybeMethod = readUntil(' '.toByte)
      if maybeMethod.isEmpty then return None
      val methodSlice = maybeMethod.get

      // 2. Extract the PATH (read until the next space).
      val maybePath = readUntil(' '.toByte)
      if maybePath.isEmpty then return None
      val pathSlice = maybePath.get

      // 3. Skip the HTTP version by reading until the end-of-line.
      while buffer.hasRemaining && buffer.get() != '\n'.toByte do ()

      // 4. Extract HEADERS.
      // Headers are assumed to end with CRLFCRLF.
      val headersStart = buffer.position()
      // We assume the underlying ByteBuffer is array-backed.
      val arr = buffer.array()
      // Calculate the array index corresponding to the current position.
      val arrOffset = buffer.arrayOffset() + buffer.position()
      var headerEnd = -1
      var i = 0
      while i <= buffer.remaining() - 4 && headerEnd == -1 do
        if arr(arrOffset + i)   == '\r'.toByte &&
          arr(arrOffset + i+1) == '\n'.toByte &&
          arr(arrOffset + i+2) == '\r'.toByte &&
          arr(arrOffset + i+3) == '\n'.toByte then
          headerEnd = buffer.position() + i
        else
          i += 1
      if headerEnd == -1 then return None
      // Make a slice for headers.
      buffer.position(headersStart)
      val headersSlice = buffer.slice()
      headersSlice.limit(headerEnd - headersStart)
      // Advance past the header terminator (\r\n\r\n).
      buffer.position(headerEnd + 4)

      // 5. The rest is the BODY.
      val bodySlice = buffer.slice()

      Some(HttpRequest(methodSlice, pathSlice, headersSlice, bodySlice))

  // Extension methods give you a nice API to work with HttpRequest.
  extension[B] (req: HttpRequest[B])
    private def methodBuffer: ByteBuffer = req._1
    private def pathBuffer: ByteBuffer = req._2
    private def headersBuffer: ByteBuffer = req._3
    private def bodyBuffer: B = req._4

    // Decode the ByteBuffer into a String. We use a duplicate in order not to disturb positions.
    def method: HttpMethod.HttpMethod =
      HttpMethod(new String(methodBuffer.duplicate().array(),
        methodBuffer.arrayOffset() + methodBuffer.position(),
        methodBuffer.remaining(),
        StandardCharsets.UTF_8))
    def path: HttpPath =
      HttpPath.apply(new String(pathBuffer.duplicate().array(),
        pathBuffer.arrayOffset() + pathBuffer.position(),
        pathBuffer.remaining(),
        StandardCharsets.UTF_8))
    def headers: String =
      new String(headersBuffer.duplicate().array(),
        headersBuffer.arrayOffset() + headersBuffer.position(),
        headersBuffer.remaining(),
        StandardCharsets.UTF_8)
    def body: B = bodyBuffer
    def parse: Option[HttpRequest[B]] = Some(req)
extension (b: java.nio.ByteBuffer) def asRequest: Option[HttpRequest.HttpRequest[ByteBuffer]] = HttpRequest.HttpRequest.fromFullBuffer(b)

