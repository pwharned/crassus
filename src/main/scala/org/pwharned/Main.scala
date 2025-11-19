package org.pwharned
import org.pwharned.http.server.HttpServer
import org.pwharned.http.dsl.{Dispatcher, InlineRouter, Route, endpoint}
import org.pwharned.http.response.HttpResponse
import org.pwharned.io.IO
import org.pwharned.http.dsl.EndpointDsl.*

object Main:
  @main def run(): Unit =
    // Define routes using your macro DSL

    val router = InlineRouter.build((      endpoint.get("/health").serverLogic { req =>
      IO.pure(HttpResponse.ok("OK"))
    },
      endpoint.get("health").serverLogic { req =>
        IO.pure(HttpResponse.ok("OK"))
      }
      ))




    // Start server - now with clean abstractions!
    HttpServer
      .builder(InlineRouter)
      .bind("0.0.0.0", 8080)
      .withBufferSize(16 * 1024)  // Optional tuning
      .withMaxBatch(256)           // Optional tuning
      .start()

    println("Server started on http://localhost:8080")
    println("Try: curl http://localhost:8080/health")
