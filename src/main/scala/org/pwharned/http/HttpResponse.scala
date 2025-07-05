package org.pwharned.http

import org.pwharned.http.Headers.Headers
import org.pwharned.json.JsonSerializer
import org.pwharned.http.Http



case class HttpResponse[T](
                            status: Int = 200,
                            headers: Headers = Headers.empty,
                            body: Body
                          )

object HttpResponse:
  /** build a JSON-encoded “200 OK” from any `T` with a `BodyEncoder[Http,T]` */
  def error[T](message: String): HttpResponse[T] =
    HttpResponse(
      status = 500,
      headers = Headers.empty,
      body = Body.text(message)
    )

  def notFound[T]: HttpResponse[T] =
    HttpResponse(
      status = 404,
      headers = Headers.empty,
      body = Body.text("404 Not Found")
    )
  /** build a JSON-encoded “200 OK” from any `T` with a `BodyEncoder[Http,T]` */
  def ok[T](entity: T, headers: Headers = Headers.empty)(using be: BodyEncoder[Http, T]): HttpResponse[T] =
    be(entity)
