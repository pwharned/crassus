package org.pwharned.http.response


import org.pwharned.codec.Codec

import java.nio.ByteBuffer
import java.nio.channels.SocketChannel

// HttpResponse no longer needs B, as EntitySerializer now writes directly to buffer.
// We'll assume the entity will be written to a ByteBuffer.
case class HttpResponse[E](
                            status: Int,
                            headers:    Seq[(String,String)],
                            entity:     E
                          ) 


object HttpResponse  {

  def apply[E](   status: Int,
               headers:    Seq[(String,String)],
               entity:     E): HttpResponse[E] = new  HttpResponse(status,  headers, entity)

  inline def render[E](writeBuf: ByteBuffer, channel: SocketChannel, response: HttpResponse[E]): Unit = {
    writeBuf.clear() // Clear the pooled buffer for a fresh response
    // 1) Calculate entity size first (may involve temporary allocation by serializer)
    val serialized  = response.entity.toString.getBytes("UTF-8")
    val contentLength = serialized.length
    writeBuf.putAscii(version).putAscii(response.status.toString).putAscii(ok).putAscii("\r\n")
    
    (response.headers :+  ("Content-Length" -> contentLength.toString) ).foreach { case (k, v) =>
      writeBuf.putAscii(k).putAscii(": ").putAscii(v).putAscii("\r\n")
    }
    writeBuf.putAscii("\r\n") // End of headers

    // 3) Write the serialized entity into the buffer, immediately after headers
    writeBuf.put(serialized)
    
    writeBuf.flip() // Prepare the entire buffer (headers + body) for reading

    // 4) Write the entire buffer to the channel in one go (if it fits)
    while (writeBuf.hasRemaining) {
      channel.write(writeBuf)
    }
  }
  implicit class ByteBufferAscii(val buf: ByteBuffer) extends AnyVal {
    inline def putAscii(s: String): ByteBuffer = {
      var i = 0
      while (i < s.length) {
        buf.put(s.charAt(i).toByte)
        i += 1
      }
      buf
    }
  }

  inline val version = "HTTP/1.1 "
  inline val ok = " OK"
  def ok(entity: String): HttpResponse[String] = new HttpResponse[String](200, Seq.empty, entity)


}
