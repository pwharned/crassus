package org.pwharned.http.dsl
import org.pwharned.http.request.HttpRequestView
import org.pwharned.http.response.HttpResponse
import org.pwharned.io.IO


object InlineRouter extends Handler:

  import scala.compiletime.{erasedValue, summonInline}

  // Match type to compute the tuple's tail at the type level
  type Tail[T <: Tuple] <: Tuple = T match
    case EmptyTuple   => EmptyTuple
    case _ *: t       => t

  trait AllRoutes[T <: Tuple]
  object AllRoutes:
    given AllRoutes[EmptyTuple] with {}
    given [H <: Route[?], T <: Tuple](using AllRoutes[T]): AllRoutes[H *: T] with {}

  private var router: String => HttpRequestView => IO[HttpResponse[?]] =
    _ => _ => IO.pure(HttpResponse(404, Nil, "Not Found"))

  def handle(req: HttpRequestView): IO[HttpResponse[?]] =
    router(req.path)(req)

  inline def build[Routes <: Tuple](inline routes: Routes)(using AllRoutes[Routes]): Unit =
    router = inlineDispatch[Routes](routes)

  transparent inline def inlineDispatch[Routes <: Tuple](routes: Routes)(using AllRoutes[Routes])
  : String => HttpRequestView => IO[HttpResponse[?]] =
    inline erasedValue[Routes] match
      case _: EmptyTuple =>
        _ => _ => IO.pure(HttpResponse(404, Nil, "Not Found!"))

      case _: (Route[?] *: Tail[Routes]) =>
        val tup = routes.asInstanceOf[(Route[?] *: Tail[Routes])]
        val r   = tup.head
        val rs  = tup.tail

        // Derive evidence for the tail via the inductive given
        given AllRoutes[Tail[Routes]] = summonInline[AllRoutes[Tail[Routes]]]

        path => req =>
          if path == r.path then
          r.logic(req)
          else
            inlineDispatch[Tail[Routes]](rs)(path)(req)
