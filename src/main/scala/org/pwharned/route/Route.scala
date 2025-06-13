package org.pwharned.route

import org.pwharned.http.HttpMethod.HttpMethod
import org.pwharned.http.HttpPath.HttpPath
import org.pwharned.http.HttpRequest.HttpRequest
import org.pwharned.http.HttpResponse
import org.pwharned.route.Router.Route

import java.nio.ByteBuffer
import java.nio.channels.SocketChannel
import scala.concurrent.{ExecutionContext, Future}

object Router:


  // A case class wrapping metadata plus a handler function which now needs an HttpRequest.

  // Route now includes connection handling logic
  case class Route[+F[_], +T <: HttpMethod](
                                           method: T,
                                           path: HttpPath,
                                           handler: HttpRequest => Future[HttpResponse]
                                         )(using writer: SocketWriter[F], connection: ConnectionHandler[F]) {

    def processRequest(socket: SocketChannel, request: HttpRequest)(using ec: ExecutionContext): Future[Unit] = {
      for {
        response <- handler(request)
        _ <- writer.write(socket, response)
      } yield connection.handleConnection(socket)

    }
  }
  // The opaque type Route now is backed by RouteDef.


  // DSL for creating a route.
  inline def route[F[_],T <: HttpMethod](method: T, path: HttpPath, f: HttpRequest => Future[HttpResponse])(using ec: ExecutionContext,s: SocketWriter[F], c: ConnectionHandler[F]): Route[F, T] =
    Route(method, path, f)

  given [F[_], T <: HttpMethod](using SocketWriter[Http], ConnectionHandler[Http]): Conversion[Route[F, T], Route[Http, T]] =
    route => Route(route.method, route.path, req => route.handler(req))


  // Extensions to "unwrap" our opaque type so we can use it as a function and also access its metadata.
  object Route:
    extension [F[_],T <: HttpMethod](r: Route[F,T])
      def apply(request: HttpRequest): Future[HttpResponse] = r.handler(request)

    extension [F[_], T <: HttpMethod](r: Route[F,T])
      def method: T = r.method

    extension [F[_], T <: HttpMethod](r: Route[F,T])
      def path: HttpPath = r.path

sealed trait Protocal[F]

sealed trait SSE[F] extends Protocal[F]
sealed trait Http[F] extends Protocal[F]


trait SocketWriter[F[_]] {
  def write(socket: SocketChannel, response: HttpResponse)(implicit ec: ExecutionContext): Future[Unit]
}


// Implement type class instances
given sseWriter: SocketWriter[SSE] with
  def write(socket: java.nio.channels.SocketChannel,
            response: HttpResponse)
           (using ExecutionContext): Future[Unit] =

    // Mandatory SSE headers
    val head =
      s"HTTP/1.1 ${response.status} OK\r\n" +
        "Content-Type: text/event-stream\r\n" +
        "Cache-Control: no-cache\r\n" +
        "Connection: keep-alive\r\n" +
        response.headers.asMap.map((k,v) => s"$k: $v\r\n").mkString +
        "\r\n"

    val headFuture = Future {
      val buf = ByteBuffer.wrap(head.getBytes("UTF-8"))
      while buf.hasRemaining do socket.write(buf)
      socket.socket().getOutputStream.flush()
    }

    // Initial body (could be Strict with an opening event or empty)
    for _ <- headFuture
        _ <- response.body.writeTo(socket)
    yield ()                 // do *not* close the socket


given httpWriter: SocketWriter[Http] with
  def write(socket: java.nio.channels.SocketChannel,
            response: HttpResponse)
           (using ExecutionContext): Future[Unit] =
    // 1) HTTP head
    val head =
      s"HTTP/1.1 ${response.status} OK\r\n" +
        response.headers.asMap.map((k, v) => s"$k: $v\r\n").mkString +
        "\r\n"

    val headFuture = Future {
      val buf = ByteBuffer.wrap(head.getBytes("UTF-8"))
      while buf.hasRemaining do socket.write(buf)
    }

    for
      _ <- headFuture
      _ <- response.body.writeTo(socket)
    yield socket.close()

trait ConnectionHandler[F[_]] {
  def handleConnection(socket: SocketChannel): Unit
}

// Implement connection behaviors
given sseConnection: ConnectionHandler[SSE] with {
  def handleConnection(socket: SocketChannel): Unit = {
    // SSE keeps the connection open for event streaming
    socket.close()
    
  }
}

given httpConnection: ConnectionHandler[Http] with {
  def handleConnection(socket: SocketChannel): Unit = {
    // HTTP closes the connection after response
  socket.close()
  }
}

// Route automatically resolves the correct SocketWriter[F]

// DSL for creating a Route with implicit resolution of SocketWriter and ConnectionHandler
inline def route[F[_], T <: HttpMethod](
                                         method: T,
                                         path: HttpPath,
                                         f: HttpRequest => Future[HttpResponse]
                                       )(using s: SocketWriter[F] = httpWriter, c: ConnectionHandler[F] = httpConnection,ec: ExecutionContext): Route[F, T] =
  Route(method, path, f)

// --- Usage ---
//inline def labels[Labels <: Tuple](using ev: Tuple.Union[Labels] <:< String): List[String] = ev.substituteCo(constValueTuple[Labels].toList)

