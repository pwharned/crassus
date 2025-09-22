package org.pwharned.http.server.dsl

import org.pwharned.http.request.HttpRequestView
import org.pwharned.http.response.{EntitySerializer, HttpResponse}
import org.pwharned.io.IO

// Route now directly produces HttpResponse[E]
case class Route[E](
                     method: String,
                     path: String,
                     logic: HttpRequestView => IO[HttpResponse[E]]
                   ) {
}

object Route {
  // Helper methods to define routes more ergonomically
  // The 'using' clause is implicitly passed through to the HttpResponse constructor
  def get[E](path: String)(logic: HttpRequestView => IO[HttpResponse[E]]): Route[E] =
    Route("GET", path, logic)

  def post[E](path: String)(logic: HttpRequestView => IO[HttpResponse[E]]): Route[E] =
    Route("POST", path, logic)

  // ... add other HTTP methods as needed
}
