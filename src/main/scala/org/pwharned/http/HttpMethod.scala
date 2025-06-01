package org.pwharned.http



object HttpMethod:
  opaque type GET = String
  opaque type POST = String
  opaque type PATCH = String
  opaque type PUT = String
  opaque type DELETE = String
  type HttpMethod = GET | POST | PATCH | PUT | DELETE
  val GET: GET = "GET"
  val POST: POST = "POST"
  val PUT: PUT = "PUT"
  val PATCH: PATCH = "PATCH"
  val DELETE: DELETE = "DELETE"

  def apply(method: String): HttpMethod = method
  
  extension (m: HttpMethod) def asString: String = m