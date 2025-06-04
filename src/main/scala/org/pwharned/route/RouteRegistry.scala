package org.pwharned.route

import org.pwharned.database.HKD.*
import org.pwharned.http.HttpMethod.HttpMethod
import org.pwharned.http.{Creatable, Deletable, Retrievable, Updatable}
import org.pwharned.route.Router.Route

import scala.concurrent.ExecutionContext

object RouteRegistry:
  def getRoutes[T[F[_]] <: Product](using Retrievable[T[Id]], Creatable[Persisted[T]], Updatable[Updated[T]], Deletable[T[Id]], ExecutionContext): List[Route[HttpMethod]] =
    List(summon[Retrievable[T[Id]]].get, summon[Creatable[Persisted[T]]].post, summon[Updatable[Updated[T]]].update, summon[Deletable[T[Id]]].delete, summon[Retrievable[T[Id]]].getWhere )
