package org.pwharned.http

import org.pwharned.http.Headers.Headers

case class HttpResponse(
                         status:  Int                    = 200,
                         headers: Headers                = Headers.empty,
                         body:    Body                   = Body.text("")   // default: empty 200
                       )

object HttpResponse:
  def ok(text: String, headers: Headers = Headers.empty): HttpResponse =
    HttpResponse(200, headers, Body.text(text))

  def error(message: String): HttpResponse =
    HttpResponse(500, Headers.empty, Body.text(message))

  def notFound(): HttpResponse =
    HttpResponse(404, Headers.empty, Body.text("404 Not Found"))
