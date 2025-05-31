package org.pwharned.route

import org.pwharned.http.{HttpRequest, HttpResponse}

import scala.concurrent.Future

object Router:
  opaque type GET = String
  opaque type POST = String
  opaque type PATCH = String
  opaque type PUT = String
  opaque type DELETE = String
  type HttpMethod = GET | POST | PATCH | PUT | DELETE

  // A case class wrapping metadata plus a handler function which now needs an HttpRequest.
  case class RouteDef[T <: HttpMethod](method: T, path: String, handler: HttpRequest => Future[HttpResponse])

  // The opaque type Route now is backed by RouteDef.
  opaque type Route[T <: HttpMethod] = RouteDef[T]

  object HttpMethod:
    val GET: GET = "GET"
    val POST: POST = "POST"
    val PUT: PUT = "PUT"
    val PATCH: PATCH = "PATCH"
    val DELETE: DELETE = "DELETE"

  // DSL for creating a route.
  inline def route[T <: HttpMethod](method: T, path: String, f: HttpRequest => Future[HttpResponse]): Route[T] =
    RouteDef(method, path, f)

  // Extensions to "unwrap" our opaque type so we can use it as a function and also access its metadata.
  object Route:
    extension [T <: HttpMethod](r: Route[T])
      def apply(request: HttpRequest): Future[HttpResponse] = r.handler(request)

    extension [T <: HttpMethod](r: Route[T])
      def method: T = r.method

    extension [T <: HttpMethod](r: Route[T])
      def path: String = r.path

// --- Usage ---
//inline def labels[Labels <: Tuple](using ev: Tuple.Union[Labels] <:< String): List[String] = ev.substituteCo(constValueTuple[Labels].toList)

