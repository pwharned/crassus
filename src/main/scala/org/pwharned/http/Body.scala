package org.pwharned.http

import java.nio.ByteBuffer
import scala.concurrent.{ExecutionContext, Future}

/** Algebra for writing a body to the socket. */
sealed trait Body:
  /** Copy all remaining bytes into the socket. */
  def writeTo(socket: java.nio.channels.SocketChannel)
             (using ExecutionContext): Future[Unit]

object Body:

  /** A finite, already-known payload.  After it is written the caller may
   * safely close the connection. */
  final case class Strict(bytes: Array[Byte]) extends Body:
    def writeTo(socket: java.nio.channels.SocketChannel)
               (using ExecutionContext): Future[Unit] = Future {
      val buf = ByteBuffer.wrap(bytes)
      while buf.hasRemaining do socket.write(buf)
    }

  /** An open-ended producer.  Each callback call must push *one* message
   * (`Array[Byte]`) to the socket; it returns `false` when the stream is
   * finished.  The socket is **not** closed by default.                    */
  final case class Streamed(nextChunk: () => Option[Array[Byte]]) extends Body:

    def writeTo(socket: java.nio.channels.SocketChannel)
               (using ExecutionContext): Future[Unit] = Future {
      var opt = nextChunk()
      while opt.nonEmpty do
        val buf = ByteBuffer.wrap(opt.get)
        while buf.hasRemaining do socket.write(buf)
        opt = nextChunk()
    }

  /** Helper for simple text bodies. */
  def text(str: String, charset: String = "UTF-8"): Body =
    Strict(str.getBytes(charset))
