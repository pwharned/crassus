package org.pwharned.http.server

import org.pwharned.http.dsl.Handler
import org.pwharned.http.request.{HttpParser, HttpRequestView}
import org.pwharned.http.response.{EntityWriter, HttpResponse}
import org.pwharned.http.server.tcp.*
import org.pwharned.io.IO

import java.nio.ByteBuffer
import java.nio.channels.SocketChannel

class HttpProtocol(router: Handler) extends Protocol[HttpRequestView, HttpResponse[String]]:

  inline def parser: RequestParser[HttpRequestView] = {
    val parser = new HttpParser()
    new RequestParser[HttpRequestView] {
      override def feed(buffer: ByteBuffer): Unit = parser.feed(buffer)

      override def take(): Option[HttpRequestView] = parser.take()
    }
  }

  def handler: RequestHandler[HttpRequestView, HttpResponse[String]] =
    new RequestHandler[HttpRequestView, HttpResponse[String]]:
      def handle(request: HttpRequestView): HttpResponse[String] =
        router.handle(request).unsafeRunOptimized() match
          case response: HttpResponse[String] => response

  inline def renderer: ResponseRenderer[HttpResponse[String]] =
    (response: HttpResponse[String], buffer: ByteBuffer, channel: SocketChannel) => HttpResponse.render(buffer, channel, response)

// Rest stays the same...
object HttpServer:
  class Builder(handler: Handler):
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
      given protocol: Protocol[HttpRequestView, HttpResponse[String]] =
        new HttpProtocol(handler):
          override def bufferSize: Int = Builder.this.bufferSize
          override def maxBatch: Int = Builder.this.maxBatch

      val server = new TcpServer[HttpRequestView, HttpResponse[String]](port)
      server.start()

  def builder(handler: Handler): Builder = new Builder(handler)
