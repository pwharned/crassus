package org.pwharned
import org.pwharned.codec.Codec
import org.pwharned.http.{HttpRequest, HttpResponse}
import org.pwharned.http.HttpTypes.ByteSlice
import org.pwharned.http.server.TypedHttpServer
import org.pwharned.io.IO

import scala.language.implicitConversions
import scala.util.Try


  




@main
def main(): Unit =

  // Domain model
  case class User(id: Long, name: String, email: String)
  case class CreateUserRequest(name: String, email: String)

  // Simple JSON codec for demonstration
  given userCodec: Codec[User] with
    def decode(slice: ByteSlice): Either[String, User] =
      // Simplified - would use proper JSON library
      val json = slice.toString
      Try {
        // Parse JSON manually for demo
        User(1L, "John", "john@example.com")
      }.toEither.left.map(_.getMessage)

    def encode(user: User): Array[Byte] =
      s"""{"id":${user.id},"name":"${user.name}","email":"${user.email}"}""".getBytes()

    def contentType: String = "application/json"



  val server = TypedHttpServer(8080)

  // Type-safe routes with opaque types - zero runtime cost!
  server.get("/") { (_: HttpRequest[Unit]) =>
    IO.pure(HttpResponse.ok("Welcome to the API"))
  }

  server.get("/users/1") { (_: HttpRequest[Unit]) =>
    val user = User(1L, "John Doe", "john@example.com")
    IO.pure(HttpResponse.ok(user))
  }
  
  


  

  // Start the server
  server.start().unsafeRun()

  
  