package org.pwharned.route

import org.pwharned.http.HttpMethod.HttpMethod
import org.pwharned.http.HttpPath.HttpPath
import org.pwharned.http.HttpRequest.HttpRequest
import org.pwharned.http.HttpResponse
import org.pwharned.openapi.{Schema, mediaType, operation, pathItem, response, schema}
import org.pwharned.route.Router.Route
import org.pwharned.http.SocketWriter
import org.pwharned.http.{Http, SSE, httpWriter}

import scala.compiletime.erasedValue
import org.pwharned.macros.{simpleTypeName, typeName}

import java.nio.channels.SocketChannel
import scala.compiletime.summonInline
import scala.concurrent.{ExecutionContext, Future}

object Router:


  // A case class wrapping metadata plus a handler function which now needs an HttpRequest.

  // Route now includes connection handling logic
  case class Route[+F[_], +T <: HttpMethod, A](
                                           method: T,
                                           path: HttpPath,
                                           handler: HttpRequest => Future[HttpResponse[A]],
                                           pathItem:  pathItem
                                         )(using writer: SocketWriter[F], connection: ConnectionHandler[F]) {
    type Out = A
    inline def responseSchema: schema = summonInline[Schema[A]].toSchema

    def processRequest(socket: SocketChannel, request: HttpRequest)(using ec: ExecutionContext): Future[Unit] = {
      for {
        response <- handler(request)
        _ <- writer.write(socket, response)
      } yield connection.handleConnection(socket)

    }
  }
  // The opaque type Route now is backed by RouteDef.


  // DSL for creating a route.
  inline def route[F[_],T <: HttpMethod,A](method: T, path: HttpPath, f: HttpRequest => Future[HttpResponse[A]])(using ec: ExecutionContext,s: SocketWriter[F], c: ConnectionHandler[F], sch: Schema[A]): Route[F, T, A] =
    Route(method, path, f)

  given [F[_], T <: HttpMethod, A](using SocketWriter[Http], ConnectionHandler[Http], Schema[A]): Conversion[Route[F, T, A], Route[Http, T, A]] =
    route => Route(route.method, route.path, req => route.handler(req))


  // Extensions to "unwrap" our opaque type so we can use it as a function and also access its metadata.
  object Route:
    inline def apply[F[_], T <: HttpMethod, A](method: T, path: HttpPath, f: HttpRequest => Future[HttpResponse[A]])( using s: SocketWriter[F], c: ConnectionHandler[F], sch: Schema[A]): Route[F, T, A] = {

      val m = simpleTypeName[T]
      val returnType = simpleTypeName[A]
      val summary = s"${m.toLowerCase} a ${returnType.toLowerCase}"
      val operationId= s"${m.toLowerCase}_${returnType.toLowerCase}"
      val mediaType = new mediaType(schema = sch.toSchema)
      val res = response("Successful operation", headers = None, content = Some(Map("application/json" -> mediaType)))
      val operation = new operation(
        summary = summary , operationId = operationId, tags = Nil, parameters = None, responses = Map("200" -> res,       "default" -> response(description = "Error", headers=None, content=None)
        )
      )
      val PathItem = m match {
        case "GET" =>  pathItem(get = Some(operation))
        case "POST" =>  pathItem(post = Some(operation))
      }
      Route(method, path, f, pathItem = PathItem)
    }

    extension [F[_],T <: HttpMethod, A](r: Route[F,T, A])
      def apply(request: HttpRequest): Future[HttpResponse[A]] = r.handler(request)

  extension [F[_], M<:HttpMethod ](rs: List[Route[F, M, ?]])
    def schemas: List[pathItem] = rs.map( x=> x.pathItem )


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
inline def route[F[_], T <: HttpMethod, A](
                                         method: T,
                                         path: HttpPath,
                                         f: HttpRequest => Future[HttpResponse[A]]
                                       )(using s: SocketWriter[F] = httpWriter, c: ConnectionHandler[F] = httpConnection,ec: ExecutionContext, sch: Schema[A]): Route[F, T, A] =
  Route(method, path, f)



