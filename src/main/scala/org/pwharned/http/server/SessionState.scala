package org.pwharned.http.server

import org.pwharned.http.request.HttpParser
import org.pwharned.http.response.{EchoParser}

import java.nio.ByteBuffer
import java.nio.channels.{ClosedChannelException, SelectionKey, SocketChannel}

class SessionState( val key: SelectionKey, val channel: SocketChannel,
                          private val readBuf: ByteBuffer,
                          private val writeBuf: ByteBuffer) {
  val parser = new EchoParser

  @inline def onReadable(): Unit = {
    val httpResponse = "HTTP/1.1 200 OK\r\n" + "Content-Type: text/plain\r\n" + "Content-Length: 13\r\n" + "\r\n" + "Hello, world!"
    try {
      readBuf.clear()
      val n = channel.read(readBuf)
      if (n > 0) { readBuf.flip()
        writeBuf.clear()
        writeBuf.put(httpResponse.getBytes("UTF-8"))
        writeBuf.flip()
        while (writeBuf.hasRemaining)
          channel.write(writeBuf)
      } else if (n == -1) {
        channel.close()
        key.cancel()
      }
    } catch {
      case _: ClosedChannelException => key.cancel()
      case e: Exception =>
        println(s"[onReadable] ${e.getClass.getName}: ${e.getMessage}")
        e.printStackTrace()
        channel.close()
        key.cancel()
    }
  }
}
object SessionState {

  inline def newSession(ch: SocketChannel, key: SelectionKey, bufferPools:Map[String, BufferPool]): SessionState = {

    val pool = selectPool(ch, bufferPools)
    val readBuf = pool.allocateReadBuffer()
    val writeBuf = pool.allocateWriteBuffer()
    new SessionState(key, ch, readBuf, writeBuf)
  }

  inline def selectPool(ch: SocketChannel, bufferPools:Map[String, BufferPool]): BufferPool = {
    val hash = ch.getRemoteAddress.toString.hashCode
    val key = if ((hash & 1) == 0) "small" else "large"
    bufferPools.getOrElse(key, BufferPool.heap(4096))
  }
}