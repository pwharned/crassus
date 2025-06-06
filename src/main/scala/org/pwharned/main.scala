package org.pwharned

import generated.user
import org.pwharned.database.{Db2TypeMapper, DbTypeMapper, PrimaryKeyExtractor}
import org.pwharned.http.HttpMethod.{GET, HttpMethod}
import org.pwharned.http.HttpRequest.HttpRequest
import org.pwharned.http.{HttpResponse, Segment, asPath}
import org.pwharned.database.HKD.{Id, New, Updated}
import org.pwharned.route.Router.{Route, route}
import org.pwharned.route.RoutingTable.RoutingTable
import org.pwharned.route.{Http, Protocal, RouteRegistry, RoutingTable, SSE, httpConnection, httpWriter, sseConnection, sseWriter}
import org.pwharned.server.HTTPServer

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

@main
def main(): Unit =

  
  given ExecutionContext = ExecutionContext.fromExecutor(Executors.newVirtualThreadPerTaskExecutor())
  given DbTypeMapper = Db2TypeMapper


  inline def r: Route[Http, GET] = route[Http,GET](GET, "/health/ping".asPath, (req: HttpRequest) => Future{ HttpResponse.ok("Ok")})


  inline def userRoutes: List[Route[Protocal, org.pwharned.http.HttpMethod.HttpMethod]] = RouteRegistry.getRoutes[user, Protocal,HttpMethod]
  inline def table: RoutingTable[Segment, Protocal] = RoutingTable.build[Segment, Protocal](userRoutes)
  println(table)
  HTTPServer.start(8080, table)








