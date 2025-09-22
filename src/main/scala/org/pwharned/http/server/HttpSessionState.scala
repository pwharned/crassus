package org.pwharned.http.server

import org.pwharned.http.request.{HttpParser, HttpRequestView}
import org.pwharned.http.response.HttpResponse
import org.pwharned.io.IO

import java.nio.ByteBuffer
import java.nio.channels.{ClosedChannelException, SelectionKey, SocketChannel}
import org.pwharned.http.response.EntitySerializer.stringEntitySerializer
import org.pwharned.http.server.dsl.Handler

private class HttpSessionState(
                                key:       SelectionKey,
                                channel:   SocketChannel,
                                readBuf:   ByteBuffer,
                                writeBuf:  ByteBuffer,
                                // Handler now returns HttpResponse[?]
                                router:   Handler
                              ) extends SessionState(key, channel, readBuf, writeBuf) {
  private val parser: HttpParser = new HttpParser()

  @inline override def onReadable(): Unit = {
    try {
      readBuf.clear()
      val n = channel.read(readBuf)
      if (n > 0) {
        readBuf.flip()
        parser.feed(readBuf)

        parser.take() match {
          case Some(requestView) => {

            val ioResponse = router.handle(requestView)
            ioResponse.unsafeRunOptimized() match {
              case response: HttpResponse[?] =>
                // Now, call the render method directly on the HttpResponse instance
                HttpResponse.render(writeBuf, channel, response)
            }
          }
          case None => {
            // println("STUB: HttpSessionState - Malformed request. Returning 400.") // Removed for performance
            // Handle malformed request - create HttpResponse directly
            val malformedResponse = HttpResponse("HTTP/1.1 400 Bad Request", Seq("Content-Type" -> "text/plain"), "Bad Request!")
            HttpResponse.render(writeBuf, channel, malformedResponse)
          }
        }
      } else if (n == -1) {
        // println("STUB: HttpSessionState - Channel closed by client.") // Removed for performance
        channel.close()
        key.cancel()
      }
    } catch {
      case _: ClosedChannelException =>
        // println("STUB: HttpSessionState - Channel closed due to ClosedChannelException.") // Removed for performance
        key.cancel()
      case e: Exception =>
        println(s"[onReadable] ${e.getClass.getName}: ${e.getMessage}")
        e.printStackTrace()
        channel.close()
        key.cancel()
    }
  }
}

object HttpSessionState {
  inline def newSession(
                         ch: SocketChannel,
                         key: SelectionKey,
                         bufferPools: Map[String, BufferPool],
                         // Handler now returns HttpResponse[?]
                         router: Handler
                       ): HttpSessionState = {
    val pool     = selectPool(ch, bufferPools)
    val readBuf  = pool.allocateReadBuffer()
    val writeBuf = pool.allocateWriteBuffer()
    new HttpSessionState(key, ch, readBuf, writeBuf, router)
  }

  inline def selectPool(ch: SocketChannel, bufferPools:Map[String, BufferPool]): BufferPool = {
    val hash = ch.getRemoteAddress.toString.hashCode
    val key = if ((hash & 1) == 0) "small" else "large"
    bufferPools.getOrElse(key, BufferPool.heap(4096))
  }
}
