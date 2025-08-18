package org.pwharned.route


import scala.concurrent.{ExecutionContext, Future}
import org.pwharned.http.HttpMethod.HttpMethod
import org.pwharned.http.HttpPath.HttpPath
import org.pwharned.http.HttpRequest.HttpRequest
import org.pwharned.http.{Body, Headers, HttpResponse}

trait ResourceAuthorizer[M <: HttpMethod,Req, Res] {

  /** now always works on HttpRequest[Any] */
  def authorize(path: HttpPath)
               (req: HttpRequest[Req])
               (using ec: ExecutionContext)
  : Future[Boolean]

  /** emits Middleware[Any,Res]—which you can drop into Middleware[Req,Res] */
  def middleware(path: HttpPath)
                (using ec: ExecutionContext)
  : Middleware[Req, Res] =
    next => req =>
      authorize(path)(req).flatMap {
        case true  => next(req)
        case false => Future.successful(
          HttpResponse(403, Headers.empty, Body.text("Forbidden"))
        )
      }
}
