package org.pwharned.macros

import org.pwharned.http.HttpMethod.HttpMethod
import org.pwharned.macros.HKD.Id
import org.pwharned.route.Router.Route

import scala.concurrent.ExecutionContext

object RouteRegistry:
  def getRoutes[T[F[_]] <: Product](using Updatable[T[Id]], ExecutionContext): List[Route[HttpMethod]] =
    List(summon[Updatable[T[Id]]].update)
