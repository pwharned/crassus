package org.pwharned.server

import scala.concurrent.Future

type Handler = HTTPRequest => Future[HTTPResponse]
