package org.pwharned.http

import java.nio.ByteBuffer
import java.nio.channels.SocketChannel
import scala.concurrent.{ExecutionContext, Future}

trait SocketWriter[F[_]] {
  def write[A](socket: SocketChannel, response: HttpResponse[A])(implicit ec: ExecutionContext): Future[Unit]
}

object SocketWriter:
  given sseWriter: SocketWriter[SSE] with
    def write[A](socket: java.nio.channels.SocketChannel,
                 response: HttpResponse[A])
                (using ExecutionContext): Future[Unit] =
  
      // Mandatory SSE headers
      val head =
        s"HTTP/1.1 ${response.status} OK\r\n" +
          "Content-Type: text/event-stream\r\n" +
          "Cache-Control: no-cache\r\n" +
          "Connection: keep-alive\r\n" +
          response.headers.asMap.map((k,v) => s"$k: $v\r\n").mkString +
          "\r\n"
  
      val headFuture = Future {
        val buf = ByteBuffer.wrap(head.getBytes("UTF-8"))
        while buf.hasRemaining do socket.write(buf)
        socket.socket().getOutputStream.flush()
      }
  
      // Initial body (could be Strict with an opening event or empty)
      for _ <- headFuture
          _ <- response.body.writeTo(socket)
      yield ()                 // do *not* close the socket
  
  
  given httpWriter: SocketWriter[Http] with
    def write[A](socket: java.nio.channels.SocketChannel,
                 response: HttpResponse[A])
                (using ExecutionContext): Future[Unit] =
      // 1) HTTP head
      val head =
        s"HTTP/1.1 ${response.status} OK\r\n" +
          response.headers.asMap.map((k, v) => s"$k: $v\r\n").mkString +
          "\r\n"
  
      val headFuture = Future {
        val buf = ByteBuffer.wrap(head.getBytes("UTF-8"))
        while buf.hasRemaining do socket.write(buf)
      }
  
      for
        _ <- headFuture
        _ <- response.body.writeTo(socket)
      yield socket.close()