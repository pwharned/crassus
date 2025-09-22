package org.pwharned.http.server.dsl
import org.pwharned.experiments.PrintTree.printTree
import org.pwharned.http.request.HttpRequestView
import org.pwharned.http.response.EntitySerializer.stringEntitySerializer
import org.pwharned.http.response.HttpResponse
import org.pwharned.io.IO

import scala.quoted.*


object InlineRouter extends Handler:

  def handle(req: HttpRequestView): IO[HttpResponse[?]] = {
    
    val path = req.path
    router.apply(path).logic(req)

  }
  private var router: (String => Route[?]) = (str:String) =>  Route.get("/hello") { (req: HttpRequestView) =>
    IO.pure(new HttpResponse("HTTP/1.1 200 OK", Seq.empty, "Hello from Scala 3!"))
  }

  inline def build(
                                  inline Routes: Route[? <: Any]*
                                ): Unit = router = Dispatcher.dispatchRoutePathFn(Routes*)
    
    
  /**
   * 1) Immediate dispatch: inline key + nested-match tree at the call site
   */
  
end InlineRouter