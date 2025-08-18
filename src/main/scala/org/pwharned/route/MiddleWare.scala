package org.pwharned.route

import org.pwharned.http.HttpRequest.HttpRequest
import org.pwharned.http.HttpResponse

import scala.concurrent.Future

// A pure request ⇒ response function
type Handler[Req, Res] = HttpRequest[Req] => Future[HttpResponse[Res]]

// A middleware wraps one Handler into another
type Middleware[Req, Res] =
  Handler[Req, Res] => Handler[Req, Res]
