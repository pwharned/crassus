package org.pwharned.http.server.tcp

import java.nio.ByteBuffer
import java.nio.channels.SocketChannel

trait RequestParser[Request]:
  def feed(buffer: ByteBuffer): Unit
  def take(): Option[Request]

trait RequestHandler[Request, Response]:
  def handle(request: Request): Response

// Simplified - just needs to write response
trait ResponseRenderer[Response]:
  def render(response: Response, buffer: ByteBuffer, channel: SocketChannel): Unit

trait Protocol[Req, Resp]:
  type Request = Req
  type Response = Resp

  def parser: RequestParser[Request]
  def handler: RequestHandler[Request, Response]
  def renderer: ResponseRenderer[Response]

  def bufferSize: Int = 8192
  def maxBatch: Int = 128

  def onConnectionOpen(channel: SocketChannel): Unit = ()
  def onConnectionClose(channel: SocketChannel): Unit = ()
