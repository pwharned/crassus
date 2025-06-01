package org.pwharned.http
import org.pwharned.http.HttpMethod.HttpMethod

import java.nio.ByteBuffer

case class HttpRequest(method: HttpMethod, path: String, headers: Map[String, String], body: ByteBuffer)





object HttpParser:
  def parseRequest(buffer: ByteBuffer): Option[HttpRequest] =
    scala.util.Try{val method = extractMethod(buffer)
    val path = extractPath(buffer)
    val headers = extractHeaders(buffer)
    val body = extractBody(buffer)
    HttpRequest(HttpMethod(method), path, headers, body)}.toOption

  private def extractMethod(buffer: ByteBuffer): String =
    val start = buffer.position()
    while buffer.hasRemaining && buffer.get() != ' ' do ()
    val end = buffer.position() - 1
    new String(buffer.array(), start, end - start)

  private def extractPath(buffer: ByteBuffer): String =
    val start = buffer.position()
    while buffer.hasRemaining && buffer.get() != ' ' do ()
    val end = buffer.position() - 1
    new String(buffer.array(), start, end - start)

  private def extractHeaders(buffer: ByteBuffer): Map[String, String] =
    var headers = Map.empty[String, String]
    while buffer.hasRemaining && buffer.get() != '\r' do
      val keyStart = buffer.position() - 1
      while buffer.hasRemaining && buffer.get() != ':' do ()
      val keyEnd = buffer.position() - 1
      buffer.get() // Skip space
      val valueStart = buffer.position()
      while buffer.hasRemaining && buffer.get() != '\r' do ()
      val valueEnd = buffer.position() - 1
      headers += new String(buffer.array(), keyStart, keyEnd - keyStart) ->
        new String(buffer.array(), valueStart, valueEnd - valueStart)
      buffer.get() // Skip newline
    buffer.get() // Skip newline after headers
    headers

  private def extractBody(buffer: ByteBuffer): ByteBuffer =
    buffer.slice()