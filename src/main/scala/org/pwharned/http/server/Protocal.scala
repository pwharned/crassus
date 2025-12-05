package org.pwharned.http.server.tcp

import java.nio.ByteBuffer
import java.nio.channels.SocketChannel

trait RequestParser[Request]:
  def feed(buffer: ByteBuffer): Unit
  def take(): Option[Request]

trait RequestHandler[Request]:
  def handle(request: Request, buffer: ByteBuffer, channel: SocketChannel): Unit

trait Protocol[Req]:
  type Request = Req

  def parser: RequestParser[Request]
  def handler: RequestHandler[Request]

  def bufferSize: Int = 8192
  def maxBatch: Int = 128

  def onConnectionOpen(channel: SocketChannel): Unit = ()
  def onConnectionClose(channel: SocketChannel): Unit = ()
