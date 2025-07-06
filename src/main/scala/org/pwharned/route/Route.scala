package org.pwharned.route

import org.pwharned.http.HttpMethod.HttpMethod
import org.pwharned.http.HttpPath.HttpPath
import org.pwharned.http.HttpRequest.HttpRequest
import org.pwharned.http.{BodyReader, Http, HttpResponse, SSE, SocketWriter, httpWriter}
import org.pwharned.openapi.{Schema, mediaType, operation, pathItem, response, schema}
import org.pwharned.route.Router.Route

import scala.compiletime.erasedValue
import org.pwharned.macros.{simpleTypeName, typeName}

import java.nio.ByteBuffer
import java.nio.channels.SocketChannel
import scala.compiletime.summonInline
import scala.concurrent.{ExecutionContext, Future}

object Router:


  // A case class wrapping metadata plus a handler function which now needs an HttpRequest.

  // Route now includes connection handling logic
  case class Route[+F[_], +T <: HttpMethod, Req, Res](
                                           method: T,
                                           path: HttpPath,
                                           handler:  HttpRequest[Req] => Future[HttpResponse[Res]],
                                           pathItem:  pathItem
                                         )(using writer: SocketWriter[F], connection: ConnectionHandler[F], bodyReader: BodyReader[Req]) {
    type Out = Res
    inline def responseSchema: schema = summonInline[Schema[Res]].toSchema

    def processRequest(socket: SocketChannel, request: HttpRequest[ByteBuffer])(using ec: ExecutionContext ): Future[Unit] = {

      request.as[Req] match {
        // JSON‐parse failure → 400
        case Left(err) =>
          val bad = HttpResponse.error( s"Bad Request – cannot parse JSON: ${err}")
          writer.write(socket, bad)
            .map(_ => connection.handleConnection(socket))

        // Parsed OK → invoke user handler then write & close
        case Right(typedReq) =>
          handler(typedReq)
            .flatMap(resp => writer.write(socket, resp))
            .map(_ => connection.handleConnection(socket))
      }

    }
  }





  // DSL for creating a route.
  inline def route[F[_],T <: HttpMethod,Req: BodyReader, Res](method: T, path: HttpPath, f: HttpRequest[Req] => Future[HttpResponse[Res]])(using ec: ExecutionContext,s: SocketWriter[F], c: ConnectionHandler[F], sch: Schema[Res]): Route[F, T, Req, Res] =
    Route(method, path, f)

  given [F[_], T <: HttpMethod, Req: BodyReader, Res](using SocketWriter[Http], ConnectionHandler[Http], Schema[Res]): Conversion[Route[F, T, Req, Res], Route[Http, T, Req, Res]] =
    route => Route(route.method, route.path, req => route.handler(req))


  // Extensions to "unwrap" our opaque type so we can use it as a function and also access its metadata.
  object Route:
    inline def apply[F[_], T <: HttpMethod, Req: BodyReader, Res](method: T, path: HttpPath, f: HttpRequest[Req] => Future[HttpResponse[Res]])( using s: SocketWriter[F], c: ConnectionHandler[F], sch: Schema[Res]): Route[F, T, Req, Res] = {

      val m = simpleTypeName[T]
      val returnType = simpleTypeName[Res]
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

    extension [F[_],T <: HttpMethod, Req, Res](r: Route[F,T, Req, Res])
      def apply(request: HttpRequest[Req]): Future[HttpResponse[Res]] = r.handler(request)

  extension [F[_], M<:HttpMethod ](rs: List[Route[F, M, ?, ?]])
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
inline def route[F[_], T <: HttpMethod, Req: BodyReader, Res](
                                         method: T,
                                         path: HttpPath,
                                         f: HttpRequest[Req] => Future[HttpResponse[Res]]
                                       )(using s: SocketWriter[F] = httpWriter, c: ConnectionHandler[F] = httpConnection,ec: ExecutionContext, sch: Schema[Res]): Route[F, T, Req, Res] =
  Route(method, path, f)



