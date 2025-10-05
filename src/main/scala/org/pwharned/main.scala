package org.pwharned

import org.pwharned.http.response.HttpResponse
import org.pwharned.http.server.HttpServer
import org.pwharned.http.server.dsl.{InlineRouter, endpoint}
import org.pwharned.http.server.dsl.Macros.*
import org.pwharned.http.server.dsl.endpoints.Dispatcher2
import org.pwharned.io.*

import scala.language.implicitConversions






@main def runHttp(): Unit = {
  // 1) Define your routing logic


  inline def e1 = endpoint.get("/page").serverLogic(
     x => IO.pure(HttpResponse.ok("Ok") )
   )

  inline def e2 = endpoint.get("/page/2").serverLogic(
    x => IO.pure(HttpResponse.ok("Ok"))
  )

  val router = InlineRouter
  router.build(e1, e2)


  HttpServer.builder(router).bind("0.0.0.0", 8080).start()










}