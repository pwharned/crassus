package org.pwharned
import org.pwharned.codec.Codec.*
import org.pwharned.http.{HttpRequest, HttpResponse}
import org.pwharned.http.HttpTypes.ByteSlice
import org.pwharned.http.server.{HttpServer, TypedHttpServer}
import org.pwharned.io.IO

import scala.language.implicitConversions
import scala.util.Try
import org.pwharned.codec.JsonDecoder.*
import org.pwharned.codec.JsonEncoder.*

import scala.deriving.Mirror






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

  
  