package org.pwharned.http



case class HttpRequest(method: String, path: String, headers: Map[String, String], body: String)


