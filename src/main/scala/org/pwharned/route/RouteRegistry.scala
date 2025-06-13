package org.pwharned.route

import org.pwharned.database.{Database, SqlSelect, retrieve}
import org.pwharned.database.HKD.*
import org.pwharned.http.HttpMethod.{GET, HttpMethod}
import org.pwharned.http.{BodyEncoder, Creatable, Deletable, HttpResponse, Retrievable, Updatable, toPath}
import org.pwharned.route.Router.Route
import org.pwharned.json.JsonSerializer

import scala.concurrent.Future
import scala.compiletime.constValue
import scala.concurrent.ExecutionContext
import scala.deriving.Mirror
import scala.util.{Failure, Success, Try}

private def toResponse[A](fa: Future[Try[A]])
                         (encode: A => HttpResponse)
                         (using ExecutionContext): Future[HttpResponse] =

  fa.map {
    case Failure(exception) => HttpResponse.error(exception.getMessage)
    case Success(value) => encode(value)
  }
object RouteRegistry:

  inline def list[P[_], T[_[_]]<: Product](using // ← T now has correct kind
                                         db: Database.type,
                                         sql: SqlSelect[Persisted[T]], // SqlSelect[UserPersisted]
                                         enc: BodyEncoder[P, Iterator[Persisted[T]]],
                                         sw: SocketWriter[P],
                                         ch: ConnectionHandler[P],
                                         ec: ExecutionContext,
                                         m: Mirror.ProductOf[Persisted[T]]
                                        ): Route[P, GET] =
    
    val table = constValue[m.MirroredLabel]
    route(GET, s"/api/$table".toPath, _ =>
      toResponse(db.retrieve[T])(enc.apply)       // <- single line!
    )
    