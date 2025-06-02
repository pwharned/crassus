package org.pwharned

import generated.{PrimaryKey, user}
import org.pwharned.http.HttpMethod.{GET, HttpMethod}
import org.pwharned.http.HttpPath
import org.pwharned.http.HttpRequest.HttpRequest
import org.pwharned.http.{HttpPath, HttpRequest, HttpResponse}
import org.pwharned.macros.{Db2TypeMapper, DbTypeMapper, RouteRegistry}
import org.pwharned.route.Router.{Route, route}
import org.pwharned.route.RoutingTable
import org.pwharned.server.HTTPServer
import org.pwharned.macros.asPath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

@main
def main(): Unit =

  
  given ExecutionContext = ExecutionContext.fromExecutor(Executors.newVirtualThreadPerTaskExecutor())
  given DbTypeMapper = Db2TypeMapper


  inline def r: Route[HttpMethod] = route(GET, "/health/ping/{ping_id}/details/{details_id}".asPath, (req: HttpRequest) => Future(HttpResponse.ok("Ok")))
  inline def userRoutes =RouteRegistry.getRoutes[user]
  inline def table: RoutingTable.RoutingTable = RoutingTable.build(List(r))
  println(table.find(GET, "/health/ping/1/details/1".asPath))

  HTTPServer.start(8080, table)









