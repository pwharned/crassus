package org.pwharned.http.response

import org.pwharned.http.response.HttpResponse

import java.nio.ByteBuffer
import java.nio.channels.SocketChannel

object WriteResponse {
  // zero-alloc ASCII writer
  implicit class ByteBufferAscii(val buf: ByteBuffer) extends AnyVal {
    def putAscii(s: String): ByteBuffer = {
      var i = 0
      while (i < s.length) {
        buf.put(s.charAt(i).toByte)
        i += 1
      }
      buf
    }
  }
  def writeResponse[E, B](
                           resp:     HttpResponse[E],
                           writeBuf: ByteBuffer,
                           channel:  SocketChannel
                         )(
                           implicit
                           S: EntitySerializer[E],
                           R: BodyRenderer[B]
                         ): Unit = {
    // 1) statusLine + headers
    writeBuf.clear()
    writeBuf.putAscii(resp.statusLine).putAscii("\r\n")
    (resp.headers ++ S.headers(resp.entity.toString.length)).foreach { case (k,v) =>
      writeBuf.putAscii(k).putAscii(": ").putAscii(v).putAscii("\r\n")
    }
    writeBuf.putAscii("\r\n")
    writeBuf.flip()

    // 2) body via compile-time BodyRenderer
    //val bodyContainer = S.serialize(resp.entity)
    //R.renderBody(bodyContainer, writeBuf, channel)
  }
}
