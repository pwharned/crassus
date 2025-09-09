package org.pwharned.http.server

import org.pwharned.codec.Codec
import org.pwharned.http.HttpTypes.HttpPath
import org.pwharned.http.{BufferPool, HttpMethod, HttpRequest, HttpResponse, ResponseData, Route, RouteRegistry, StatusCode}
import org.pwharned.io.IO

import java.nio.ByteBuffer

class TypedHttpServer(port: Int):
  private val server = Server(port)
  private val registry = new RouteRegistry()

  // Type-safe route registration with opaque types
  def route[A, B](method: HttpMethod, path: HttpPath)(
    handler: HttpRequest[A] => IO[HttpResponse[B]]
  )(using Codec[A], Codec[B]): Unit =
    registry.register(Route(method, path, handler))

  // Convenience methods with opaque types
  def get[B](path: String)(handler: HttpRequest[Unit] => IO[HttpResponse[B]])(using Codec[B]): Unit =
    route(HttpMethod.GET, HttpPath(path))(handler)

  def post[A, B](path: String)(handler: HttpRequest[A] => IO[HttpResponse[B]])(using Codec[A], Codec[B]): Unit =
    route(HttpMethod.POST, HttpPath(path))(handler)

  def put[A, B](path: String)(handler: HttpRequest[A] => IO[HttpResponse[B]])(using Codec[A], Codec[B]): Unit =
    route(HttpMethod.PUT, HttpPath(path))(handler)

  def delete[B](path: String)(handler: HttpRequest[Unit] => IO[HttpResponse[B]])(using Codec[B]): Unit =
    route(HttpMethod.DELETE, HttpPath(path))(handler)

  def start(): IO[Unit] =
    for
      _ <- server.bind()
      _ <- IO.println(s"Server started on port $port")
     // _ <- handleConnections()
    yield ()

  private def handleConnections(): IO[Unit] =
    server.acceptStream()
      .map(_.flatMap(handleConnection))
      .take(1000) // Limit concurrent connections
      .fold(IO.pure(())) { (acc, connectionIO) =>
        acc.flatMap(_ => connectionIO)
      }

  private def handleConnection(conn: Connection): IO[Unit] =
    for
      buffer <- IO.effect(BufferPool.acquire())
      _ <- conn.read(buffer)
      _ <- IO.effect(buffer.flip())
      result <- routeRequest(buffer, conn)
      _ <- IO.effect(BufferPool.release(buffer))
      _ <- conn.close()
    yield ()

  private def routeRequest(buffer: ByteBuffer, conn: Connection): IO[Unit] =
    // Parse raw request first to get method and path
    parseRawRequest(buffer).flatMap {
      case Left(error) =>
        writeErrorResponse(conn, StatusCode.BadRequest, error)
      case Right((method, path)) =>
        registry.findRoute(method, path) match
          case None =>
            writeErrorResponse(conn, StatusCode.NotFound, "Route not found")
          case Some(route) =>
            handleTypedRoute(route, buffer, conn)
    }

  private def parseRawRequest(buffer: ByteBuffer): IO[Either[String, (HttpMethod, HttpPath)]] =
    IO.effect {
      val requestLineEnd = HttpRequest.findSequence(buffer, "\r\n".getBytes(), 0)
      if requestLineEnd == -1 then return IO.pure(Left("Invalid request line"))

      val requestLine = Array.ofDim[Byte](requestLineEnd)
      buffer.position(0)
      buffer.get(requestLine)
      val requestStr = new String(requestLine)
      val parts = requestStr.split(" ")

      if parts.length >= 2 then
        Right((HttpMethod(parts(0)), HttpPath(parts(1))))
      else
        Left("Invalid request line format")
    }

  private def handleTypedRoute(route: Route[?, ?], buffer: ByteBuffer, conn: Connection): IO[Unit] =
    // Type erasure handling - in real implementation would use TypeTag or similar
    route match
      case r: Route[a, b] =>
        given requestCodec: Codec[a] = r.requestCodec
        given responseCodec: Codec[b] = r.responseCodec

        HttpRequest.parse[a](buffer).flatMap {
          case Left(error) =>
            writeErrorResponse(conn, StatusCode.BadRequest, error)
          case Right(request) =>
            r.handler(request).flatMap { response =>
              writeTypedResponse(conn, response)
            }
        }

  private def writeTypedResponse[A](conn: Connection, response: HttpResponse[A])(using codec: Codec[A]): IO[Unit] =
    val buffer = ByteBuffer.wrap(response.toBytes)
    conn.write(buffer).map(_ => ())

  private def writeErrorResponse(conn: Connection, status: StatusCode, message: String): IO[Unit] =
    val responseOpaque = HttpResponse.internalError(message)
    writeTypedResponse(conn, responseOpaque)(using Codec.stringCodec)
