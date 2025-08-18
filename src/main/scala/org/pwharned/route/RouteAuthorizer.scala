package org.pwharned.route


import org.pwharned.http.{Body, Headers, HttpResponse}
import org.pwharned.http.HttpMethod.HttpMethod
import org.pwharned.http.HttpPath.HttpPath
import org.pwharned.http.HttpRequest.HttpRequest

import scala.concurrent.{ExecutionContext, Future}

/** Summons a Policy[R,M] and runs it on user alone (collection‐level) */
trait RouteAuthorizer[M <: HttpMethod, Req, Res]{

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
