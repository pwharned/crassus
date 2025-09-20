package org.pwharned.http

import HttpTypes.*
import org.pwharned.codec.Codec
import org.pwharned.io.IO

import java.nio.ByteBuffer
import scala.annotation.tailrec
import scala.collection.mutable

// Internal data structure for HTTP request
case class HttpRequest[A](
                           rawData: ByteBuffer,
                           requestLineEnd: Int,
                           headersEnd: Int,
                           bodyStart: Int,
                           bodyValue: A,
                           // Cached values for lazy parsing
                           var cachedMethod: Option[HttpMethod]=None,
                           var cachedPath: Option[HttpPath]=None,
                           var cachedHeaders: Map[HeaderName, ByteSlice] = null,
                           var cachedQueryParams: Map[String, ByteSlice] = null
                         )

// Zero-cost opaque HTTP request with full type safety

object HttpRequest:
  // Constructor - only way to create HttpRequest

  // Parse from buffer with typed body
  def parse[A](buffer: ByteBuffer)(using codec: Codec[A]): IO[Either[String, HttpRequest[A]]] =
    IO.effect {
      val requestLineEnd = findSequence(buffer, "\r\n".getBytes(), 0)
      if requestLineEnd == -1 then return IO.pure(Left("Invalid request line"))

      val headersEnd = findSequence(buffer, "\r\n\r\n".getBytes(), requestLineEnd)
      if headersEnd == -1 then return IO.pure(Left("Invalid headers"))

      val bodyStart = headersEnd + 4
      val bodySlice = if bodyStart < buffer.limit() then
        ByteSlice(buffer, bodyStart, buffer.limit())
      else
        ByteSlice(buffer, 0, 0)

      codec.decode(bodySlice) match
        case Left(error) => Left(s"Body decode error: $error")
        case Right(body) =>
          val data = HttpRequest(buffer, requestLineEnd, headersEnd, bodyStart, body)
          Right(data)
    }

  def parseRawRequest(buffer: ByteBuffer): Either[String, (HttpMethod, HttpPath)] =
    val requestLineEnd = HttpRequest.findSequence(buffer, "\r\n".getBytes(), 0)
    if (requestLineEnd == -1) {
      Left("Invalid request line")
    } else {
      val requestLine = Array.ofDim[Byte](requestLineEnd)
      val originalPos = buffer.position()
      buffer.position(0)
      buffer.get(requestLine)
      buffer.position(originalPos)

      val requestStr = new String(requestLine)
      val parts = requestStr.split(" ")

      if (parts.length >= 2) {
        Right((HttpMethod(parts(0)), HttpPath(parts(1))))
      } else {
        Left("Invalid request line format")
      }
    }
  def parseTypedRequest[A](buffer: ByteBuffer)(using codec: Codec[A]): Either[String, HttpRequest[A]] =
    HttpRequest.parse[A](buffer).unsafeRun()
  def findSequence(buffer: ByteBuffer, sequence: Array[Byte], start: Int): Int =
    val limit = buffer.limit() - sequence.length + 1
    var i = start

    while i < limit do
      var matches = true
      for j <- sequence.indices if matches do
        if buffer.get(i + j) != sequence(j) then
          matches = false

      if matches then return i
      i += 1

    -1

  // Extension methods for zero-cost operations
  extension [A](request: HttpRequest[A])
    // Direct access to underlying data - zero cost

    // Zero-allocation method access with lazy parsing
    inline def method: HttpMethod =
      if Option(request.cachedMethod).isEmpty then parseRequestLine(request)
      request.cachedMethod.get

    inline def path: HttpPath =
      if Option(request.cachedPath).isEmpty then parseRequestLine(request)
      request.cachedPath.get

    // Direct typed body access - zero additional cost
    inline def body: A = request.bodyValue

    // Zero-copy header access with lazy parsing
    def headers: Map[HeaderName, ByteSlice] =
      if request.cachedHeaders == null then parseHeaders(request)
      request.cachedHeaders

    inline def header(name: HeaderName): Option[ByteSlice] =
      headers.get(name)

    // Optimized common header accessors
    inline def contentType: Option[String] =
      header(HeaderName.ContentType).map(_.toString)

    inline def contentLength: Option[Long] =
      header(HeaderName.ContentLength).map(_.toString.toLong)

    inline def isChunked: Boolean =
      header(HeaderName.TransferEncoding)
        .exists(_.toString.toLowerCase.contains("chunked"))

    // Query parameters with zero-copy values and lazy parsing
    def queryParams: Map[String, ByteSlice] =
      if request.cachedQueryParams == null then parseQueryParams(request)
      request.cachedQueryParams

    inline def queryParam(name: String): Option[ByteSlice] =
      queryParams.get(name)

    // Zero-cost transformation to different body type
    def as[B](newBody: B): HttpRequest[B] =
      request.copy(bodyValue = newBody)

    // Zero-copy body slice access
    def bodySlice(): ByteSlice = ByteSlice(request.rawData, request.bodyStart, request.rawData.limit())

    // Streaming body with backpressure
    def bodyStream(chunkSize: Int = 8192): org.pwharned.stream.Stream[ByteSlice] =
      if request.bodyStart >= request.rawData.limit() then org.pwharned.stream.Empty
      else
        org.pwharned.stream.Stream.unfold(request.bodyStart) { offset =>
          if offset >= request.rawData.limit() then None
          else
            val end = (offset + chunkSize).min(request.rawData.limit())
            val slice = ByteSlice(request.rawData, offset, end)
            Some((slice, end))
        }

  // Private parsing methods with maximum efficiency
  private def parseRequestLine[A](data: HttpRequest[A]): Unit =
    var pos = 0
    var spaceCount = 0
    var methodEnd = 0
    var pathEnd = 0

    while pos < data.requestLineEnd do
      if data.rawData.get(pos) == ' ' then
        spaceCount match
          case 0 =>
            methodEnd = pos
            spaceCount = 1
          case 1 =>
            pathEnd = pos
            spaceCount = 2
          case _ => // ignore
      pos += 1

    // Extract method
    val methodBytes = Array.ofDim[Byte](methodEnd)
    val originalPos = data.rawData.position()
    data.rawData.position(0)
    data.rawData.get(methodBytes)
    data.cachedMethod = Some(HttpMethod(new String(methodBytes)))

    // Extract path  
    val pathBytes = Array.ofDim[Byte](pathEnd - methodEnd - 1)
    data.rawData.position(methodEnd + 1)
    data.rawData.get(pathBytes)
    data.cachedPath = Some(HttpPath(new String(pathBytes)))

    data.rawData.position(originalPos)

  private def parseHeaders[A]( data: HttpRequest[A]): Unit =
    val headers = mutable.Map[HeaderName, ByteSlice]()
    var lineStart = data.requestLineEnd + 2 // Skip \r\n

    while lineStart < data.headersEnd do
      val lineEnd = findLineEnd(data, lineStart)
      if lineEnd > lineStart then
        val colonPos = findColon(data, lineStart, lineEnd)
        if colonPos > lineStart then
          val nameLength = colonPos - lineStart
          val nameBytes = Array.ofDim[Byte](nameLength)
          val pos = data.rawData.position()
          data.rawData.position(lineStart)
          data.rawData.get(nameBytes)
          data.rawData.position(pos)

          val headerName = HeaderName(new String(nameBytes).toLowerCase)
          val valueStart = skipSpaces(data, colonPos + 1, lineEnd)
          val valueSlice = ByteSlice(data.rawData, valueStart, lineEnd)
          headers(headerName) = valueSlice

      lineStart = lineEnd + 2

    data.cachedHeaders = headers.toMap

  private def parseQueryParams[A](data: HttpRequest[A]): Unit =
    // Ensure path is parsed first
    if data.cachedPath == null then parseRequestLine(data)

    data.cachedPath.map( x=> x.queryString) match
      case None =>
        data.cachedQueryParams = Map.empty
      case Some(queryStr) =>
        val params = mutable.Map[String, ByteSlice]()
        val pairs = queryStr.map( x=> x.split("&")).get

        pairs.foreach { pair =>
          val eqIndex = pair.indexOf("=")
          if eqIndex > 0 then
            val name = pair.substring(0, eqIndex)
            val value = pair.substring(eqIndex + 1)
            val valueBytes = value.getBytes()
            val buffer = ByteBuffer.wrap(valueBytes)
            params(name) = ByteSlice(buffer, 0, valueBytes.length)
          else if pair.nonEmpty then
            val emptyBuffer = ByteBuffer.allocate(0)
            params(pair) = ByteSlice(emptyBuffer, 0, 0)
        }

        data.cachedQueryParams = params.toMap

  // Optimized byte scanning methods
  private  def findLineEnd[A](data: HttpRequest[A], start: Int): Int =
    var i = start
    while i < data.headersEnd - 1 do
      if data.rawData.get(i) == '\r' && data.rawData.get(i + 1) == '\n' then
        return i
      i += 1
    data.headersEnd

  private  def findColon[A](data: HttpRequest[A], start: Int, end: Int): Int =
    var i = start
    while i < end do
      if (data.rawData.get(i) == ':'){
        return 1
      } 
      i += 1
    -1

  private inline def skipSpaces[A](data: HttpRequest[A], start: Int, end: Int): Int =
    var i = start
    while i < end && data.rawData.get(i) == ' ' do i += 1
    i

