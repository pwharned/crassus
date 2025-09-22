package org.pwharned.http.server

import org.pwharned.http.request.HttpRequestView
import org.pwharned.http.response.HttpResponse
import org.pwharned.http.server.dsl.{Handler, InlineRouter}
import org.pwharned.io.IO

import java.nio.channels.SelectionKey

object HttpServer {

  class Builder(handler: Handler ) {
    private var host: String = "0.0.0.0"
    private var port: Int = 8080

    // Handler now returns IO[HttpResponse[?]]

    def bind(host: String, port: Int): Builder =
      this.host = host;
      this.port = port;
      this

    // The withHandler method now takes a handler returning HttpResponse[?]


    val acceptFactory: (SelectionKey, BufferPoolCollection) => HttpAcceptEvent =
      (key, pools) =>
        HttpAcceptEvent(
          key,
          pools,
          handler // Pass the updated handler
        )

    def start(): Unit = TcpServer.run(port, acceptFactory, HttpReadEvent.apply)
  }

  def builder(h: Handler): Builder = new Builder(h)
}
