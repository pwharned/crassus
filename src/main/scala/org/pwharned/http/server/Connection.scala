package org.pwharned.http.server

import org.pwharned.http.HttpRequest
import org.pwharned.io.IO

import java.nio.ByteBuffer
import java.nio.channels.{SelectionKey, SocketChannel}

case class Connection(channel: SocketChannel):
  def read(buffer: ByteBuffer): IO[Int] =
    IO.effect(channel.read(buffer))

  def write(buffer: ByteBuffer): IO[Int] =
    IO.effect(channel.write(buffer))

  def close(): IO[Unit] =
    IO.effect(channel.close())

  def isConnected: IO[Boolean] =
    IO.effect(channel.isConnected)

  def getRemoteAddress: IO[String] =
    IO.effect(channel.getRemoteAddress.toString)

// Connection state for tracking HTTP request/response lifecycle
sealed trait ConnectionState
object ConnectionState:
  case class Reading(buffer: ByteBuffer, accumulated: ByteBuffer) extends ConnectionState
  //case class Processing[A](request: HttpRequest[A]) extends ConnectionState
  case class Writing(response: ByteBuffer) extends ConnectionState

  case class Processing(buffer: ByteBuffer) extends ConnectionState

  case object Closed extends ConnectionState
