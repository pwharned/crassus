
package org.pwharned.route

import org.pwharned.macros.buildRoutingTable
import org.pwharned.route.Router.{HttpMethod, Route}

// Inline routing table builder: routes must be provided as a literal list.
import org.pwharned.route.Router




object RoutingTable:

  // Create a unique key for each route from its method and path.
  private def routeKey(method: String, path: String): String = s"$method:$path"
  type RoutingTableType = Map[String, Router.Route[HttpMethod]]
  inline def compile(inline routes: List[Router.Route[HttpMethod]]): RoutingTableType = buildRoutingTable(routes)

  // Build a single-level map: Map[String, Route]
  def build(routes: List[Router.Route[HttpMethod]]): RoutingTableType =
    routes.foldLeft(Map.empty[String, Router.Route[HttpMethod]]) { (acc, route) =>
      val key = routeKey(route.method.toString, route.path)
      acc.updated(key, route)
    }

  // Expose the same key function for use in the server.
  def keyFor(method: String, path: String): String = routeKey(method, path)
