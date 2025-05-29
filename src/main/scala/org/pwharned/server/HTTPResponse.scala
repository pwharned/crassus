package org.pwharned.server

case class HTTPResponse(status: Int, headers: Map[String, String], body: String)

object HTTPResponse:
  def ok(body: String): HTTPResponse = HTTPResponse(200, Map("Content-Type" -> "text/plain"), body)

  def error(body: String): HTTPResponse = HTTPResponse(500, Map("Content-Type" -> "text/plain"), body)

  def notFound(): HTTPResponse = HTTPResponse(404, Map("Content-Type" -> "text/plain"), "Not Found")
