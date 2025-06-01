package org.pwharned

import generated.{PrimaryKey, user}
import org.pwharned.macros.{Db2TypeMapper, DbTypeMapper, RouteRegistry}
import org.pwharned.route.RoutingTable
import org.pwharned.server.HTTPServer

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

@main
def main(): Unit =

  
  // Compose routes using the '~' operator; note that the result is a tuple.
  given ExecutionContext = ExecutionContext.fromExecutor(Executors.newVirtualThreadPerTaskExecutor())
  given DbTypeMapper = Db2TypeMapper
  
  
  val table: RoutingTable.RoutingTableType = RoutingTable.build(RouteRegistry.getRoutes[user])
  
  HTTPServer.start(8080, table)









