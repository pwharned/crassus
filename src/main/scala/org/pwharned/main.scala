package org.pwharned

import generated.user
import org.pwharned.database.{Database, Db2TypeMapper, DbTypeMapper, PrimaryKeyExtractor}
import org.pwharned.http.HttpMethod.{GET, HttpMethod}
import org.pwharned.http.HttpRequest.HttpRequest
import org.pwharned.http.{BodyEncoder, HttpResponse, Segment, asPath}
import org.pwharned.database.HKD.{Id, New, Updated}
import org.pwharned.route.Router.{Route, route}
import org.pwharned.route.RoutingTable.RoutingTable
import org.pwharned.route.{Http, Protocal, RouteRegistry, RoutingTable, SSE, httpConnection, httpWriter, sseConnection, sseWriter}
import org.pwharned.server.HTTPServer
import org.pwharned.http.jsonArrayEncoder
import org.pwharned.http.toPath
import org.pwharned.http.{jsonIteratorEncoder, sseIteratorEncoder} // <── brings the generic encoders in scope
import scala.language.implicitConversions
import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

@main
def main(): Unit =

  
  given ExecutionContext = ExecutionContext.fromExecutor(Executors.newVirtualThreadPerTaskExecutor())
  given DbTypeMapper = Db2TypeMapper


  //inline def r: Route[Http, GET] = route[Http,GET](GET, "/health/ping".asPath, (req: HttpRequest) => Future{ HttpResponse.ok("Ok")})
  inline def r = route[SSE,GET](GET, "/health/ping".asPath, (req: HttpRequest) => Future{ HttpResponse.ok("Ok")})

  // HTTP: one JSON array
  given Database.type = Database




  inline def getUsers = RouteRegistry.list[SSE, user]

  inline def getUsersByPK = RouteRegistry.listWhere[SSE, user]

  inline def createUsers = RouteRegistry.create[SSE, user]
  inline def deleteUsers = RouteRegistry.delete[Http, user]
  inline def updateUsers = RouteRegistry.update[Http, user]


  inline def routes: List[Route[Protocal, org.pwharned.http.HttpMethod.HttpMethod]] = List( getUsersByPK, getUsers, createUsers, deleteUsers, updateUsers) :+ r

  inline def table: RoutingTable[Segment, Protocal] = RoutingTable.build[Segment, Protocal](routes)
  println(table)
  HTTPServer.start(8080, table)








