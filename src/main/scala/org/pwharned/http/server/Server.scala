package org.pwharned.http.server

import org.pwharned.io.IO
import org.pwharned.stream.Stream
import java.net.InetSocketAddress
import java.nio.channels.ServerSocketChannel


class Server(port: Int):
  private val serverChannel = ServerSocketChannel.open()

  def bind(): IO[Unit] = IO.effect {
    serverChannel.bind(InetSocketAddress(port))
    serverChannel.configureBlocking(false)
  }

  def accept(): IO[Option[Connection]] = IO.effect {
    Option(serverChannel.accept()).map(Connection(_))
  }

  def acceptStream(): Stream[IO[Connection]] =
    Stream.unfold(()) { _ =>
      Some((waitForConnection(), ()))
    }

  private def waitForConnection(): IO[Connection] =
    accept().flatMap {
      case Some(conn) => IO.pure(conn)
      case None =>
        IO.effect(Thread.sleep(1)).flatMap(_ => waitForConnection())
    }

  def close(): IO[Unit] = IO.effect(serverChannel.close())
