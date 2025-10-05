package org.pwharned.http.server.dsl
import org.pwharned.http.request.HttpRequestView
import org.pwharned.http.response.HttpResponse
import org.pwharned.io.IO


object InlineRouter extends Handler:

  def handle(req: HttpRequestView): IO[HttpResponse[?]] = {
    
    val path = req.path
    router.apply(path)(req)

  }
  private var router: (String => HttpRequestView => IO[HttpResponse[?]] ) = (str:String) =>   { (req: HttpRequestView) =>
    IO.pure(new HttpResponse(200, Seq.empty, "Hello from Scala 3!"))
  }

  inline def build(
                                  inline Routes: Route[? <: Any]*
                                ): Unit = router = Dispatcher.dispatchRoutePathFn(Routes*)
    
    
  /**
   * 1) Immediate dispatch: inline key + nested-match tree at the call site
   */
  
end InlineRouter