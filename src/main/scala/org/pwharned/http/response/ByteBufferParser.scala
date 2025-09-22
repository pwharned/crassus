package org.pwharned.http.response

import org.pwharned.http.request.Parser

import java.nio.ByteBuffer

trait ByteBufferParser extends Parser[ByteBuffer]{
  @inline def feed(outBuf: ByteBuffer): Unit
}

final class EchoParser extends ByteBufferParser {
   @inline def feed( outBuf: ByteBuffer): Unit =
    val httpResponse =
      "HTTP/1.1 200 OK\r\n" +
        "Content-Type: text/plain\r\n" +
        "Content-Length: 13\r\n" +
        "\r\n" +
        "Hello, world!"
    outBuf.put(httpResponse.getBytes("UTF-8"))

  /** Once a full request is seen, returns Some(view) and clears internal state. */
  override def take(): Option[ByteBuffer] = ???
}