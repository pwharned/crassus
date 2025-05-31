package org.pwharned.http




case class HttpResponse(status: Int, headers: Map[String, String], body: String)

object HttpResponse:
  def ok(body: String): HttpResponse = new HttpResponse (headers = Map.empty, body = body, status = 200 )
  def error(message: String): HttpResponse = new HttpResponse (headers = Map.empty, body = message, status = 500 )
  def notFound(): HttpResponse =new HttpResponse (headers = Map.empty, body = "404: Not Found", status = 404 )
