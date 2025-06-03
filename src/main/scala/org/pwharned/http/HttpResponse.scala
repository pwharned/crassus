package org.pwharned.http

import org.pwharned.http.Headers.Headers


case class HttpResponse(status: Int, headers: Headers, body: String)

object HttpResponse:
  def ok(body: String, headers: Headers = Headers.empty): HttpResponse = new HttpResponse (headers = headers, body = body, status = 200 )
  def error(message: String): HttpResponse = new HttpResponse (headers = Headers.empty, body = message, status = 500 )
  def notFound(): HttpResponse =new HttpResponse (headers = Headers.empty, body = "404: Not Found", status = 404 )
