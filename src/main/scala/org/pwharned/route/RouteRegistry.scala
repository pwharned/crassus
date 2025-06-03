package org.pwharned.route

import org.pwharned.database.HKD._
import org.pwharned.http.HttpMethod.HttpMethod
import org.pwharned.http.{Creatable, Retrievable, Updatable}
import org.pwharned.route.Router.Route

import scala.concurrent.ExecutionContext

object RouteRegistry:
  def getRoutes[T[F[_]] <: Product](using Retrievable[T[Id]], Creatable[Persisted[T]], ExecutionContext): List[Route[HttpMethod]] =
    List(summon[Retrievable[T[Id]]].get, summon[Creatable[Persisted[T]]].post )
