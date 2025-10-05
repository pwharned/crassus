package org.pwharned.http.server.dsl.endpoints


import org.pwharned.http.HttpMethods.GET
import org.pwharned.http.request.HttpRequestView
import org.pwharned.http.response.HttpResponse
import org.pwharned.http.server.dsl.Route
import org.pwharned.io.IO

import scala.quoted.*



object Dispatcher2:


  /**
   * 2) Deferred dispatch: build a String => Route function once,
   *    then call it at runtime for each incoming path
   */
  inline def dispatchRoutePathFn[T](
                                  inline Route: Route[T]
                                ):  HttpRequestView => IO[HttpResponse[T]] =
    ${ dispatchRoutePathFnImpl('Route) }

  def dispatchRoutePathFnImpl[T: Type, R: Type](
                                                 routeExpr: Expr[Route[T]]
                                               )(using Quotes) = {
    import quotes.reflect.*

    routeExpr match {
      case '{ new Route[T]($methodExpr, $pathExpr, $logicExpr) } => {
        println(s"Matched lambda with body: ${logicExpr.show}")
        logicExpr
      }
      case other => report.errorAndAbort(other.show)
    }
  }


end Dispatcher2