import org.pwharned.http.HttpMethod.GET
import org.pwharned.http.HttpPath.HttpPath
import org.pwharned.http.HttpRequest.HttpRequest
import org.pwharned.http.Segment.WildCard
import org.pwharned.http.{Http, HttpResponse}
import org.pwharned.route.Router.Route
import org.pwharned.http.asPath

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import org.pwharned.route.{RoutingTable, httpConnection}
import org.pwharned.server.FileServer
object RoutingTableTest extends App {

  inline def files = Route[Http, GET, Unit, String](GET, "/static/**".asPath, (req: HttpRequest[Unit]) => Future {
    HttpResponse.ok("Ok")
  })

  lazy val table = RoutingTable.build(List(files))
  RoutingTable.printReadable(table)
  val path: HttpPath = "/static/index.js".asPath
  val mountPoint: HttpPath = "/static".asPath
  val normalized = FileServer.normalize(path, mountPoint)
  println(normalized)
  println(table.find(GET, path))


}
