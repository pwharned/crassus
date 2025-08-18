package org.pwharned.route

import org.pwharned.http.HttpMethod.HttpMethod
import org.pwharned.http.HttpPath.HttpPath
import org.pwharned.http.HttpRequest.HttpRequest
import org.pwharned.http.*
import org.pwharned.http.Headers.Headers
import org.pwharned.macros.{extractEntityType, simpleTypeName, typeName, typeToString}
import org.pwharned.openapi.*
import java.nio.ByteBuffer
import java.nio.channels.SocketChannel
import scala.compiletime.summonInline
import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.Typeable
object Router:


  // A case class wrapping metadata plus a handler function which now needs an HttpRequest.

  // Route now includes connection handling logic
  case class Route[+F[_], +T <: HttpMethod, Req, Res](
                                                       method:    T,
                                                       path:      HttpPath,
                                                       handler:   Handler[Req, Res],
                                                       pathItem:  pathItem,
                                                       var extraHeaders: Headers             = Headers.empty,
                                                       middleware: List[Middleware[Req, Res]] = Nil
                                                     )(
                                                       using writer:     SocketWriter[F],
                                                       connection: ConnectionHandler[F],
                                                       bodyReader:  BodyReader[Req]
                                                     ) {
    // Expose `withMiddleware` for a fluent API
    def withMiddleware(mw: Middleware[Req, Res]): Route[F, T, Req, Res] =
      copy(middleware = middleware :+ mw)

    private def requireCheck[Req, Res](
                                p: HttpRequest[Req] => Future[Boolean]
                              )(using ec: ExecutionContext): Middleware[Req, Res] =
      next => req =>
        p(req).flatMap {
          case true => next(req)
          case false =>
            Future.successful(
              HttpResponse(403, Headers.empty, Body.text("Forbidden"))
            )
        }
    private def requireResourceCheck[Res](
                                        p: HttpRequest[Any] => Future[Boolean]
                                      )(using ec: ExecutionContext): Middleware[Any, Res] =
      next => req =>
        p(req).flatMap {
          case true => next(req)
          case false =>
            Future.successful(
              HttpResponse(403, Headers.empty, Body.text("Forbidden"))
            )
        }
    inline def withResourceAuthorizer[M<:HttpMethod](using ec: ExecutionContext, ra: ResourceAuthorizer[M,Req,Res]): Route[F, T, Req, Res] = {
      copy(middleware = middleware :+ ra.middleware(path) ) 
    }

    inline def withRouteAuthorizer[M<:HttpMethod](using ec: ExecutionContext, ra: RouteAuthorizer[M,Req, Res]): Route[F, T, Req, Res] = {
      
      copy(middleware = middleware :+ ra.middleware(path) )
    }

    // Build the final handler by folding in all middleware
    private def composedHandler: Handler[Req, Res] =
      middleware.foldLeft(handler) { (h, mw) => mw(h) }

    def processRequest(
                        socket:  SocketChannel,
                        request: HttpRequest[ByteBuffer]
                      )(using ec: ExecutionContext): Future[Unit] =

      request.as[Req] match {
        // JSON‐parse failure → 400
        case Left(err) =>
          val bad = HttpResponse.error(s"Bad Request – cannot parse JSON: $err")
          writer.write(socket, bad).map(_ => connection.handleConnection(socket))

        // Run the middleware-stacked handler
        case Right(typedReq) =>
          composedHandler(typedReq)                        // invoke middleware + handler
            .map(_.withHeaders(extraHeaders))              // attach route’s headers
            .flatMap(resp => writer.write(socket, resp))  // write response
            .map(_ => connection.handleConnection(socket))
      }
  }


  extension [F[_], T <: HttpMethod, Req, Res](route: Route[F, T, Req, Res])
    def withHeaders(additional: Headers):Unit =
      route.extraHeaders = route.extraHeaders.merge(additional)


  object Route:
    inline def apply[F[_], T <: HttpMethod:Typeable, Req: Schema, Res: Schema](method: T, path: HttpPath, f: HttpRequest[Req] => Future[HttpResponse[Res]])(using t: Typeable[Res],  s: SocketWriter[F], c: ConnectionHandler[F], br: BodyReader[Req]): Route[F, T, Req, Res] = {

      val reqSch: schema = summon[Schema[Req]].toSchema
      val resSch: schema = summon[Schema[Res]].toSchema


      val m = simpleTypeName[T]
      val returnType =  simpleTypeName[Res] // magic code
      val summary = s"${m.toLowerCase} a ${path.segments(1).toString}"
      val operationId= s"${m.toLowerCase}_${returnType.toLowerCase}"
      val mediaType = new mediaType(schema = resSch)
      val req: Option[request] = reqSch match {
        case x if x.`type`.isEmpty => {
          None
        }
        case x => {
          Some(request(Some("A correctly formatted request"), headers = None,  Some(Map("application/json" ->  new mediaType(schema = reqSch)))))
        }
      }
      val res = response("Successful operation", headers = None, content = Some(Map("application/json" -> mediaType )))

      val operation = new operation(
        summary = summary , operationId = operationId, tags = Nil, parameters = None, requestBody = req,  responses = Map("200" -> res,       "default" -> response(description = "Error", headers=None, content=None)
        )
      )
      val PathItem = m match {
        case "GET" =>  pathItem(get = Some(operation))
        case "POST" =>  pathItem(post = Some(operation))
        case "DELETE" =>  pathItem(delete = Some(operation))
        case "PATCH" =>  pathItem(patch = Some(operation))
        case "PUT" =>  pathItem(put = Some(operation))


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



