package org.pwharned.http.server

import org.pwharned.http.HttpTypes.HttpPath
import org.pwharned.http.{HttpMethod, Route}

import scala.collection.mutable


// Route matching with compile-time type safety
class RouteRegistry:
  private val routes = mutable.ArrayBuffer[Route[?, ?]]()

  def register[A, B](route: Route[A, B]): Unit =
    routes += route

  def findRoute(method: HttpMethod, path: HttpPath): Option[Route[?, ?]] =
    routes.find(r => r.method.value == method.value && matchesPath(r.path, path))

  private def matchesPath(routePath: HttpPath, requestPath: HttpPath): Boolean =
    // Simple exact matching - can be enhanced with path parameters
    routePath.pathOnly == requestPath.pathOnly