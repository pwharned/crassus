package org.pwharned
import org.pwharned.http.codec.Codec
import org.pwharned.http.server.HttpServer
import org.pwharned.http.dsl.{Dispatcher, Inspect, Route, endpoint}
import org.pwharned.http.response.{EntityWriter, HttpResponse}
import org.pwharned.io.IO
import org.pwharned.http.dsl.EndpointDsl.*

case class User(name: String)

object User:
  given codec: Codec[User] = Codec.entityCodec[User]
object Main:
  @main
  def main(): Unit =
    // Define routes using your macro DSL

    inline def userRoute = endpoint.get("users").serverLogic { req =>
      IO.pure(HttpResponse.ok("Hello, World!"))
    }
    val result = Inspect.inspect(userRoute)

    println(result)
    def handler = Dispatcher.build(userRoute)

    println("Main thread id = " + Thread.currentThread().threadId())

    // Start server
    HttpServer.builder(handler).bind("0.0.0.0", 8080).start()

    println("Server started on http://localhost:8080")
    println("Try: curl http://localhost:8080/health")
