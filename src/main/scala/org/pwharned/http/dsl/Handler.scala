package org.pwharned.http.dsl

import org.pwharned.http.request.HttpRequestView
import org.pwharned.http.response.HttpResponse
import org.pwharned.io.IO

trait Handler {
   def handle(req: HttpRequestView): IO[HttpResponse[?]]
}
