package org.pwharned.http.dsl

import org.pwharned.http.HttpMethods.{GET, HttpMethod, POST}
import org.pwharned.http.request.HttpRequestView
import org.pwharned.http.response.{EntityWriter, HttpResponse}
import org.pwharned.io.IO

// Pure data - no rendering logic
case class Route[E](
                     method: String,
                     path: String,
                     handler: (HttpRequestView) => IO[HttpResponse[E]],

                   )

object Route:
  def get[E](path: String)(logic: HttpRequestView => IO[HttpResponse[E]]): Route[E] =
    Route("GET", path, logic)

  def post[E](path: String)(logic: (HttpRequestView) => IO[HttpResponse[E]]): Route[E] =
    Route("POST", path, logic)
