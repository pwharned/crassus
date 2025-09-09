package org.pwharned.http.server

import org.pwharned.io.IO

import java.nio.ByteBuffer
import java.nio.channels.SocketChannel

case class Connection(channel: SocketChannel):
  def read(buffer: ByteBuffer): IO[Int] =
    IO.effect(channel.read(buffer))

  def write(buffer: ByteBuffer): IO[Int] =
    IO.effect(channel.write(buffer))

  def close(): IO[Unit] =
    IO.effect(channel.close())