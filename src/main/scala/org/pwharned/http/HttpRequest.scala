package org.pwharned.http
import org.pwharned.http.HttpMethod
import java.nio.ByteBuffer

object HttpRequest:
  opaque type HttpRequest = ByteBuffer

  def apply(buffer: ByteBuffer): HttpRequest = buffer

  extension (req: HttpRequest)
    def method: HttpMethod.HttpMethod =
      extractMethod(req)

    def path: String =
      extractPath(req)

    def headers: Map[String, String] =
      extractHeaders(req)

    def body: ByteBuffer =
      extractBody(req)

    def parse: Option[HttpRequest] =
      scala.util.Try(HttpRequest(req)).toOption

  private def extractMethod(buffer: ByteBuffer): HttpMethod.HttpMethod =
    val start = buffer.position()
    while buffer.hasRemaining && buffer.get() != ' ' do ()
    val end = if buffer.position() >0 then buffer.position()- 1 else buffer.position()

    HttpMethod(new String(buffer.array(), start, end - start))

  private def extractPath(buffer: ByteBuffer): String =
    val start = buffer.position()
    while buffer.hasRemaining && buffer.get() != ' ' do ()
    val end = if buffer.position() >0 then buffer.position()- 1 else buffer.position()
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
