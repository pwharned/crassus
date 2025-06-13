package org.pwharned.route

import org.pwharned.http.HttpResponse

import java.nio.channels.SocketChannel

// A helper to write data to a SocketChannel.
// Here we assume that our effect type F supports delaying side effects.
// (This is similar to the Async abstraction in cats-effect.)
trait Async[F[_]] {
  def delay[A](thunk: => A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def pure[A](a: A): F[A]
}

// Using extension methods to simulate the *> operator from cats.
extension [F[_]](fa: F[Unit])
  def *>[B](fb: F[B])(using A: Async[F]): F[B] =
    A.flatMap(fa)(_ => fb)

// Sends the required SSE headers over the client channel.
def sendSSEHeaders[F[_]](client: SocketChannel)(using F: Async[F]): F[Unit] = {
  val headers =
    "HTTP/1.1 200 OK\r\n" +
      "Content-Type: text/event-stream\r\n" +
      "Cache-Control: no-cache\r\n" +
      "Connection: keep-alive\r\n\r\n"
  writeAndFlush(client, headers)
}

// A low-level helper that wraps the blocking write logic in F.
def writeAndFlush[F[_]](client: SocketChannel, data: String)(using F: Async[F]): F[Unit] = {
  F.delay {
    val bytes = data.getBytes("UTF-8")
    // Write the bytes to the channel.
    client.write(java.nio.ByteBuffer.wrap(bytes))
    // Ensure data is flushed (for socket channels, you might need to flush the underlying stream).
    client.socket().getOutputStream.flush()
  }
}

// A helper to convert HttpResponse into an SSE-friendly string.
// In a real app, you would convert your data structure (e.g., JSON) into the text that
// follows the SSE protocol format (e.g., "data: ...\n\n").
def responseToSSEString(resp: HttpResponse): String =
  s"data: ${resp.toString}\n\n"
