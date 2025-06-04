package org.pwharned

import generated.user
import org.pwharned.database.{Db2TypeMapper, DbTypeMapper, PrimaryKeyExtractor}
import org.pwharned.http.HttpMethod.{GET, HttpMethod}
import org.pwharned.http.HttpRequest.HttpRequest
import org.pwharned.http.HttpResponse
import org.pwharned.database.HKD.{Id, Updated, New}
import org.pwharned.http.asPath
import org.pwharned.route.Router.{Route, route}
import org.pwharned.route.{RouteRegistry, RoutingTable}
import org.pwharned.server.HTTPServer

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

@main
def main(): Unit =

  
  given ExecutionContext = ExecutionContext.fromExecutor(Executors.newVirtualThreadPerTaskExecutor())
  given DbTypeMapper = Db2TypeMapper


  inline def r: Route[HttpMethod] = route(GET, "/health/ping".asPath, (req: HttpRequest) => Future{ HttpResponse.ok("Ok")})
  inline def userRoutes =RouteRegistry.getRoutes[user] :+ r
  inline def table: RoutingTable.RoutingTable = RoutingTable.build(userRoutes )
  println(table)
  HTTPServer.start(8080, table)








