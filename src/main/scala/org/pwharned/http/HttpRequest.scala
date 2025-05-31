package org.pwharned.http
import java.net.Socket
import java.nio.ByteBuffer
import java.io.{BufferedReader, InputStreamReader, PrintWriter}
import java.io.ByteArrayOutputStream


import java.nio.ByteBuffer
import scala.annotation.tailrec

case class HttpRequest(method: HttpMethod, path: String, headers: Map[String, String], body: ByteBuffer)

def parseRequest(socket: Socket): Option[HttpRequest] = {
  val in = BufferedReader(InputStreamReader(socket.getInputStream))
  val requestLine = in.readLine()

  if (requestLine == null) {
    return None

  }
  val requestLineString = requestLine.split(" ")
  val method = requestLineString(0)
  val path = requestLineString(1)

  var headers = Map.empty[String, String]
  var line: String = in.readLine()

  while (line != null && line.nonEmpty) {
    val parts = line.split(": ")
    headers = headers.updated(parts(0), parts(1))
    line = in.readLine()
  }

  val body = if (headers.contains("Content-Length")) {
    val length = headers("Content-Length").toInt
    val output = ByteArrayOutputStream()
    socket.getInputStream.transferTo(output)
    output.toString
  } else ""

  Some(HttpRequest(method, path, headers, ByteBuffer.wrap(body.getBytes)))
}

// Define an opaque type for request parsing
opaque type HttpMethod = String
object HttpMethod:
  def apply(method: String): HttpMethod = method
  extension (m: HttpMethod) def asString: String = m


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