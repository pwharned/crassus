package org.pwharned.route

import org.pwharned.database.{Database, PrimaryKeyExtractor, PrimaryKeyFields, SqlInsert, SqlSelect, create, retrieve, retrieveParameterized}
import org.pwharned.database.HKD.*
import org.pwharned.http.HttpMethod.{GET, HttpMethod, POST}
import org.pwharned.http.{BodyEncoder, Deletable, HttpRequest, HttpResponse, Segment, Updatable, toPath}
import org.pwharned.route.Router.Route
import org.pwharned.json.{JsonDeserializer, JsonSerializer, deserialize}
import org.pwharned.macros.toTuple
import org.pwharned.parse.{QueryDeserializer, fromQuery}

import java.nio.charset.StandardCharsets
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
                                         sqls: SqlSelect[Optional[T]], 
                                           enc: BodyEncoder[P, Iterator[Persisted[T]]],
                                         sw: SocketWriter[P],
                                           queryDeserializer: QueryDeserializer[Optional[T]],
                                         ch: ConnectionHandler[P],
                                         ec: ExecutionContext,
                                         m: Mirror.ProductOf[Persisted[T]]
                                        ): Route[P, GET] =

    val table = constValue[m.MirroredLabel]


    route(GET, s"/api/$table".toPath, (req: HttpRequest.HttpRequest) =>

      {
        val queryString = req.path.query.value
        queryString.stripMargin.fromQuery[Optional[T]] match {
          case Left(value) =>  toResponse(db.retrieve[T])(enc.apply)
          case Right(value) => toResponse(db.retrieveParameterized[Optional[T], Persisted[T]](value))(enc.apply)


        }
      }

    )

  inline def listWhere[P[_], T[_[_]] <: Product](using // ← T now has correct kind
                                            db: Database.type,
                                            sql: SqlSelect[Persisted[T]], // SqlSelect[UserPersisted]
                                            enc: BodyEncoder[P, Iterator[Persisted[T]]],
                                            sw: SocketWriter[P],
                                            ch: ConnectionHandler[P],
                                            ec: ExecutionContext,
                                            m: Mirror.ProductOf[Persisted[T]]
                                           ): Route[P, GET] =

    val table = constValue[m.MirroredLabel]
    // Use PrimaryKeyExtractor on T[Id], which is your Persisted[T]
    val primaryKeys = PrimaryKeyExtractor.getPrimaryKey[Persisted[T]].map(x => s"{$x}").mkString("/")
    val path = s"/api/$table/$primaryKeys".toPath
    val dynamicIndexes = path.segments.zipWithIndex.collect {
      case (dynamic: Segment.Dynamic, index) => index
    }
    route(GET, path, (req: HttpRequest.HttpRequest) =>
{
  val keyStrings: List[String] =
    dynamicIndexes.map(req.path.segments.collect {
      case dynamic: Segment.Static => dynamic.segment.toString
    })

  // If PrimaryKeyFields is defined for the persisted type, make sure the type uses T[Id]
  val b: PrimaryKeyFields[Persisted[T]]#Out =
    toTuple(keyStrings).asInstanceOf[PrimaryKeyFields[Persisted[T]]#Out]

  toResponse(db.retrieve[Persisted[T]](b))(enc.apply)
}
    )

  inline def create[P[_], T[_[_]] <: Product](using // ← T now has correct kind
                                                 db: Database.type,
                                                 sql: SqlSelect[Persisted[T]], // SqlSelect[UserPersisted]
                                              sqlC: SqlInsert[New[T]],
                                              enc: BodyEncoder[P, Iterator[Persisted[T]]],
                                                 sw: SocketWriter[P],
                                                 ch: ConnectionHandler[P],
                                                 ec: ExecutionContext,
                                              jds: JsonDeserializer[New[T]],
                                                 m: Mirror.ProductOf[Persisted[T]]
                                                ): Route[P, POST] =

    val table = constValue[m.MirroredLabel]
    // Use PrimaryKeyExtractor on T[Id], which is your Persisted[T]
    val path = s"/api/$table".toPath

    route(POST, path, (req: HttpRequest.HttpRequest) => {
      val bytes = new Array[Byte](req.body.remaining())
      req.body.get(bytes)
      // Decode the bytes using the desired charset.
      val s = new String(bytes, StandardCharsets.UTF_8)
      s.deserialize[New[T]] match {
        case Right(value) => toResponse(db.create[New[T], Persisted[T]](value))(enc.apply)
        case Left(exception) => Future(HttpResponse.error(exception.message))
      }


    }
    )