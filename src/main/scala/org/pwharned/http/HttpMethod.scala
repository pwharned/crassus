package org.pwharned.http

object HttpMethods:
  opaque type HttpMethod = String

  val GET: HttpMethod = "GET"
  val POST: HttpMethod = "POST"
  val PUT: HttpMethod = "PUT"
  val DELETE: HttpMethod = "DELETE"
  val HEAD: HttpMethod = "HEAD"
  val OPTIONS: HttpMethod = "OPTIONS"
  val PATCH: HttpMethod = "PATCH"

  def apply(method: String): HttpMethod = method.toUpperCase

  extension (method: HttpMethod)
    inline def value: String = method
    inline def isIdempotent: Boolean = method match
      case "GET" | "HEAD" | "PUT" | "DELETE" | "OPTIONS" => true
      case _                                             => false
    inline def expectsBody: Boolean = method match
      case "POST" | "PUT" | "PATCH" => true
      case _                        => false

// Type-safe status codes
opaque type StatusCode = Int

object StatusCode:
  val Ok: StatusCode = 200
  val Created: StatusCode = 201
  val NoContent: StatusCode = 204
  val BadRequest: StatusCode = 400
  val Unauthorized: StatusCode = 401
  val Forbidden: StatusCode = 403
  val NotFound: StatusCode = 404
  val MethodNotAllowed: StatusCode = 405
  val UnprocessableEntity: StatusCode = 422
  val InternalServerError: StatusCode = 500
  val BadGateway: StatusCode = 502
  val ServiceUnavailable: StatusCode = 503

  def apply(code: Int): StatusCode = code

  extension (status: StatusCode)
    inline def code: Int = status
    inline def isSuccess: Boolean = status >= 200 && status < 300
    inline def isRedirect: Boolean = status >= 300 && status < 400
    inline def isClientError: Boolean = status >= 400 && status < 500
    inline def isServerError: Boolean = status >= 500 && status < 600
    def reasonPhrase: String = status match
      case 200 => "OK"
      case 201 => "Created"
      case 204 => "No Content"
      case 400 => "Bad Request"
      case 401 => "Unauthorized"
      case 403 => "Forbidden"
      case 404 => "Not Found"
      case 405 => "Method Not Allowed"
      case 422 => "Unprocessable Entity"
      case 500 => "Internal Server Error"
      case 502 => "Bad Gateway"
      case 503 => "Service Unavailable"
      case _   => "Unknown"
