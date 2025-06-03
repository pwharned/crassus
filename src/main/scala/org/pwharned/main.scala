package org.pwharned

import generated.user
import org.pwharned.http.HttpMethod.{GET, HttpMethod}
import org.pwharned.http.HttpRequest.HttpRequest
import org.pwharned.http.HttpResponse
import org.pwharned.macros.HKD.Id
import org.pwharned.macros.{Db2TypeMapper, DbTypeMapper, RouteRegistry, asPath}
import org.pwharned.route.Router.{Route, route}
import org.pwharned.route.RoutingTable

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

@main
def main(): Unit =

  
  given ExecutionContext = ExecutionContext.fromExecutor(Executors.newVirtualThreadPerTaskExecutor())
  given DbTypeMapper = Db2TypeMapper


  inline def r: Route[HttpMethod] = route(GET, "/health/ping/{ping_id}/details/{details_id}".asPath, (req: HttpRequest) => Future(HttpResponse.ok("Ok")))
  inline def userRoutes =RouteRegistry.getRoutes[user[Id]]
  inline def table: RoutingTable.RoutingTable = RoutingTable.build(userRoutes)
  println(table)
 // HTTPServer.start(8080, table)








