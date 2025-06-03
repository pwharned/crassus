package org.pwharned.http

import org.pwharned.database.HKD.{Id, Persisted}
import org.pwharned.database.{Database, SqlSelect,retrieve}
import org.pwharned.http.HttpMethod.{GET, HttpMethod}
import org.pwharned.http.{Headers, HttpRequest, HttpResponse}
import org.pwharned.json.{JsonSerializer, serialize}
import org.pwharned.route.Router.{Route, route}

import scala.compiletime.constValue
import scala.concurrent.ExecutionContext
import scala.deriving.Mirror
import scala.util.{Failure, Success}


trait Retrievable[T]:
  def get(using ec: ExecutionContext): Route[HttpMethod]



object Retrievable:
  inline given derive[T[F[_]] <: Product](using m: Mirror.ProductOf[Persisted[T]], sqlSelect: SqlSelect[T[Id]], serializer: JsonSerializer[Persisted[T]]): Retrievable[T[Id]] =
    new Retrievable[T[Id]]:
      def get(using ec: ExecutionContext): Route[HttpMethod]  =
        val tableName = constValue[m.MirroredLabel]
        route(GET, s"/api/$tableName".toPath,(req: HttpRequest.HttpRequest) => {
          Database.retrieve[T].map( x => x.map(y => y.serialize) match {
            case Failure(exception) => HttpResponse.error(exception.toString)
            case Success(value) => HttpResponse.ok(value,Headers.apply(Map("content-type" -> "Application/json")))
          }      )
        }   )
