package org.pwharned
import org.pwharned.http.HttpResponse
import org.pwharned.http.server.HttpServer
import org.pwharned.io.IO

import scala.language.implicitConversions






@main
def main(): Unit =

  // Domain model
  case class User(id: Long, name: String, email: String)

  // Simple JSON codec for demonstration




  val server = HttpServer(8080)

  server.get("/") { _ =>
    IO.pure(HttpResponse.ok(""))
  }






  // Start the server
  server.start().unsafeRun()

  
  