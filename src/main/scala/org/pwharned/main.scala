package org.pwharned

import generated.{PrimaryKey, user}
import org.pwharned.http.HttpMethod.{GET, HttpMethod}
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


  val r: Route[HttpMethod] = route(GET, "/health/ping".asPath, (req: HttpRequest) => Future(HttpResponse.ok("Ok")))
  val table: RoutingTable.RoutingTable = Map.empty

  HTTPServer.start(8080, table)









