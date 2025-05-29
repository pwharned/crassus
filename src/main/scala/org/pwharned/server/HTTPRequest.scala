package org.pwharned.server

case class HTTPRequest(method: String, path: String, headers: Map[String, String], body: String)
