package org.pwharned

import org.pwharned.experiments.AnimalMacro.dispatchAnimalPathFn
import org.pwharned.experiments.PrintTree.printTree
import org.pwharned.http.request.HttpRequestView
import org.pwharned.http.response.EntitySerializer.{jsonEntitySerializer, stringEntitySerializer}
import org.pwharned.http.response.HttpResponse
import org.pwharned.http.server.HttpServer
import org.pwharned.http.server.dsl.{Dispatcher, InlineRouter, Route}
import org.pwharned.io.IO

import scala.language.implicitConversions






@main def runHttp(): Unit = {
  // 1) Define your routing logic

  val httpResponse = "HTTP/1.1 200 OK\r\n" + "Content-Type: text/plain\r\n" + "Content-Length: 13\r\n" + "\r\n" + "Hello, world!"

  val router = InlineRouter
  InlineRouter.build(new Route("GET","foo",(req: HttpRequestView) =>
    IO.pure(new HttpResponse("HTTP/1.1 200 OK", Seq.empty, "Hello from foo!"))))


  HttpServer.builder(router)
   .bind("localhost", 8080)
  .start()


}