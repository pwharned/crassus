package org.pwharned.http.server

import org.pwharned.http.request.{HttpParser, HttpRequestView}
import org.pwharned.http.server.tcp.*
import org.pwharned.io.IO

import java.nio.ByteBuffer
import java.nio.channels.SocketChannel

class HttpProtocol(
    router: (
        request: HttpRequestView,
        buffer: ByteBuffer,
        channel: SocketChannel
    ) => Unit
) extends Protocol[HttpRequestView]:

  inline def parser: RequestParser[HttpRequestView] = {
    val parser = new HttpParser()
    new RequestParser[HttpRequestView] {
      override def feed(buffer: ByteBuffer): Unit = parser.feed(buffer)
      override def take(): Option[HttpRequestView] = parser.take()
    }
  }

  def handler: RequestHandler[HttpRequestView] =
    new RequestHandler[HttpRequestView]:
      // Get the dispatcher from the router

      def handler: RequestHandler[HttpRequestView] =
        (
            request: HttpRequestView,
            buffer: ByteBuffer,
            channel: SocketChannel
        ) => router(request, buffer, channel)

      override def handle(
          request: HttpRequestView,
          buffer: ByteBuffer,
          channel: SocketChannel
      ): Unit = handler.handle(request, buffer, channel)

// Execute it (unsafeRunOptimized blocks until complete)

object HttpServer:
  class Builder(
      handler: (
          request: HttpRequestView,
          buffer: ByteBuffer,
          channel: SocketChannel
      ) => Unit
  ): // Changed parameter type
    private var host: String = "0.0.0.0"
    private var port: Int = 8080
    private var bufferSize: Int = 8192
    private var maxBatch: Int = 128

    def bind(host: String, port: Int): Builder =
      this.host = host
      this.port = port
      this

    def withBufferSize(size: Int): Builder =
      this.bufferSize = size
      this

    def withMaxBatch(batch: Int): Builder =
      this.maxBatch = batch
      this

    def start(): Unit =
      given protocol: Protocol[HttpRequestView] =
        new HttpProtocol(handler):
          override def bufferSize: Int = Builder.this.bufferSize
          override def maxBatch: Int = Builder.this.maxBatch

      val server = new TcpServer[HttpRequestView](port)
      server.start()

  def builder(
      handler: (
          request: HttpRequestView,
          buffer: ByteBuffer,
          channel: SocketChannel
      ) => Unit
  ): Builder = new Builder(handler) // Changed parameter type
