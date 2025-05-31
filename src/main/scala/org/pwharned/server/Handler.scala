package org.pwharned.server

import org.pwharned.http.{HttpRequest, HttpResponse}

import scala.concurrent.Future

type Handler = HttpRequest => Future[HttpResponse]
