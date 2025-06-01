package org.pwharned.route

import org.pwharned.http.HttpMethod.{GET, HttpMethod}
import org.pwharned.http.HttpPath.HttpPath
import org.pwharned.http.HttpRequest.HttpRequest
import org.pwharned.http.{HttpPath, HttpRequest, HttpResponse}

import scala.concurrent.{ExecutionContext, Future}

object Router:


  // A case class wrapping metadata plus a handler function which now needs an HttpRequest.
  case class RouteDef[T <: HttpMethod](method: T, path: HttpPath, handler: HttpRequest => Future[HttpResponse])

  // The opaque type Route now is backed by RouteDef.
  opaque type Route[T <: HttpMethod] = RouteDef[T]


  // DSL for creating a route.
  inline def route[T <: HttpMethod](method: T, path: HttpPath, f: HttpRequest => Future[HttpResponse])(using ec: ExecutionContext): Route[T] =
    RouteDef(method, path, f)

  // Extensions to "unwrap" our opaque type so we can use it as a function and also access its metadata.
  object Route:
    extension [T <: HttpMethod](r: Route[T])
      def apply(request: HttpRequest): Future[HttpResponse] = r.handler(request)

    extension [T <: HttpMethod](r: Route[T])
      def method: T = r.method

    extension [T <: HttpMethod](r: Route[T])
      def path: HttpPath = r.path
      
    

// --- Usage ---
//inline def labels[Labels <: Tuple](using ev: Tuple.Union[Labels] <:< String): List[String] = ev.substituteCo(constValueTuple[Labels].toList)

