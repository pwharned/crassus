package org.pwharned.http

import org.pwharned.http.HttpMethod
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

object HttpRequest:
  // The HttpRequest opaque type is now a tuple of four ByteBuffers:
  // (method, path, headers, body)
  opaque type HttpRequest = (ByteBuffer, ByteBuffer, ByteBuffer, ByteBuffer)

  object HttpRequest:
    // Construct an HttpRequest from its parts.
    def apply(
               method: ByteBuffer,
               path: ByteBuffer,
               headers: ByteBuffer,
               body: ByteBuffer
             ): HttpRequest =
      (method, path, headers, body)

    /** Parses a full HTTP request from a ByteBuffer.
     *
     * Expects the request to be in the format:
     *   METHOD SP PATH SP HTTP-Version CRLF
     *   (headers terminated by CRLFCRLF)
     *   BODY
     */
    def fromFullBuffer(buffer: ByteBuffer): Option[HttpRequest] =
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
  extension (req: HttpRequest)
    def methodBuffer: ByteBuffer = req._1
    def pathBuffer: ByteBuffer = req._2
    def headersBuffer: ByteBuffer = req._3
    def bodyBuffer: ByteBuffer = req._4

    // Decode the ByteBuffer into a String. We use a duplicate in order not to disturb positions.
    def method: HttpMethod.HttpMethod =
      HttpMethod(new String(methodBuffer.duplicate().array(),
        methodBuffer.arrayOffset() + methodBuffer.position(),
        methodBuffer.remaining(),
        StandardCharsets.UTF_8))
    def path: String =
      new String(pathBuffer.duplicate().array(),
        pathBuffer.arrayOffset() + pathBuffer.position(),
        pathBuffer.remaining(),
        StandardCharsets.UTF_8)
    def headers: String =
      new String(headersBuffer.duplicate().array(),
        headersBuffer.arrayOffset() + headersBuffer.position(),
        headersBuffer.remaining(),
        StandardCharsets.UTF_8)
    def body: ByteBuffer = bodyBuffer

    def parse: Option[HttpRequest] = Some(req)
extension (b: java.nio.ByteBuffer) def asRequest: Option[HttpRequest.HttpRequest] = HttpRequest.HttpRequest.fromFullBuffer(b)
