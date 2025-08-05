package org.pwharned.route

import org.pwharned.`lazy`.Lazy
import org.pwharned.http.HttpMethod.*
import org.pwharned.http.{BodyEncoder, HttpRequest, HttpResponse, Protocal, Segment, SocketWriter, toPath}
import org.pwharned.json.JsonDeserializer
import org.pwharned.macros.toTuple
import org.pwharned.openapi.Schema
import org.pwharned.parse.{QueryDeserializer, fromQuery}
import org.pwharned.route.Router.Route
import org.pwharned.sql.database.Connection.*
import org.pwharned.sql.database.HKD.*
import org.pwharned.sql.database.{Database, FieldBinder, Row}
import org.pwharned.sql.derive.*
import org.pwharned.sql.dialect.SqlDialect

import scala.concurrent.{ExecutionContext, Future}
import scala.deriving.Mirror
import scala.util.{Failure, Success, Try}

def toResponse[A](fa: Future[Try[A]])
                         (encode: A => HttpResponse[A])
                         (using ExecutionContext): Future[HttpResponse[A]  ]  = {

  fa.map {
    case Failure(exception) => HttpResponse.error(exception.getMessage)
    case Success(value) => encode(value)
  }
}

  def toResponse[A](fa: Try[A])
                   (encode: A => HttpResponse[A])
                   (using ExecutionContext): HttpResponse[A] = {

    fa match {
      case Failure(exception) => HttpResponse.error(exception.getMessage)
      case Success(value) => encode(value)
    }
  }

object RouteRegistry:


  def get[P[_], T[_[_]]<: Product](entityName: String)(using // ← T now has correct kind
                                          db: Database,
                                          enc: BodyEncoder[P, Iterator[Persisted[T]]],
                                          sw: SocketWriter[P],
                                          ch: ConnectionHandler[P],
                                          ec: ExecutionContext,
                                                       row: Row[Persisted[T]],
                                          m: Mirror.ProductOf[Persisted[T]],
                                   qp: QueryDeserializer[Optional[T]],
                                                       sch: Schema[Persisted[T]],
                                                       sqls: SqlSelect[Persisted[T]],
                                                       sqlo:SqlSelect[Optional[T]],
                                                       fb: FieldBinder[Optional[T]]
                                         ): Route[P, GET,  Unit, Iterator[Persisted[T]]] =

    Route.apply(GET, s"/api/$entityName".toPath, (req: HttpRequest.HttpRequest[Unit]) =>

    {

      val maybeQuery: Option[String] = Option(req.path.query.value).map(_.stripMargin) match {
        case Some(value) => value match {
          case "" => None
          case _ => Some(value)
        }
        case None => None
      }

      maybeQuery match {
        case Some(value) => value.fromQuery[Optional[T]] match {
          case Left(value) => Future(HttpResponse.error(s"Bad Request: invalid query $value"))
          case Right(value) => toResponse(db.withConnection(x => x.queryParameterized[Optional[T], Persisted[T]](value)))(enc.apply)
        }
        case None => toResponse(db.withConnection( x=> x.query[Persisted[T]]) )(enc.apply)
      }

    }

    )

  inline def getWhere[P[_], T[_[_]] <: Product](entityName: String )(using // ← T now has correct kind
                                                db: Database,
                                                enc: BodyEncoder[P, Iterator[Persisted[T]]],
                                                sw: SocketWriter[P],
                                                ch: ConnectionHandler[P],
                                                ec: ExecutionContext,
                                                m: Mirror.ProductOf[Persisted[T]],
                                                              row: Row[Persisted[T]],
                                              sch: Schema[Persisted[T]],
                                                              sqlSelect: SqlSelect[Persisted[T]]
  ): Route[P, GET, Unit, Iterator[Persisted[T]]] =
    val primaryKeys = PrimaryKeyExtractor.getPrimaryKey[Persisted[T]].map(x => s"{$x}").mkString("/")
    val path = s"/api/$entityName/$primaryKeys".toPath
    val dynamicIndexes = path.segments.zipWithIndex.collect {
      case (dynamic: Segment.Dynamic, index) => index
    }
    val parseKeys = PrimaryKeyParser.makeParser[Persisted[T]]

    Route.apply(GET, path, (req: HttpRequest.HttpRequest[Unit]) =>
    {
      val keyStrings: List[String] =
        dynamicIndexes.map(req.path.segments.collect {
          case dynamic: Segment.Static => dynamic.segment.toString
        })
      val keyTuple: PrimaryKeyFields[Persisted[T]]#Out = parseKeys(keyStrings)
      toResponse(db.withConnection(x => x.query[Persisted[T]](keyTuple)))(enc.apply)
    }
    )
  inline def post[P[_], T[_[_]] <: Product](entityName: String)(using // ← T now has correct kind
                                            db: Database,
                                            enc: BodyEncoder[P, Iterator[Persisted[T]]],
                                            sw: SocketWriter[P],
                                            ch: ConnectionHandler[P],
                                            ec: ExecutionContext,
                                                                sqlInsert: SqlInsert[New[T]],
                                            m: Mirror.ProductOf[Persisted[T]],
                                            mr: Mirror.ProductOf[New[T]]
                                           ): Route[P, POST, New[T], Iterator[Persisted[T]]] =

    // Use PrimaryKeyExtractor on T[Id], which is your Persisted[T]
    val path = s"/api/$entityName".toPath

    Route.apply(POST, path, (req: HttpRequest.HttpRequest[New[T]]) => {

      toResponse(db.withConnection(x => x.insert[New[T], Persisted[T]](req.body)))(enc.apply)


    }
    )

  inline def delete[P[_], T[_[_]] <: Product](entityName: String)(using // ← T now has correct kind
                                              db: Database,
                                              enc: BodyEncoder[P, Iterator[Persisted[T]]],
                                              sw: SocketWriter[P],
                                              ch: ConnectionHandler[P],
                                              ec: ExecutionContext,
                                              m: Mirror.ProductOf[Persisted[T]],
                                                           sch: Schema[Persisted[T]],
                                                           sqlDelete: SqlDelete[Persisted[T]]
  ): Route[P, DELETE, Unit, Iterator[Persisted[T]]] =

    val primaryKeys = PrimaryKeyExtractor.getPrimaryKey[Persisted[T]].map(x => s"{$x}").mkString("/")
    val path = s"/api/$entityName/$primaryKeys".toPath
    val dynamicIndexes = path.segments.zipWithIndex.collect {
      case (dynamic: Segment.Dynamic, index) => index
    }
    val parseKeys = PrimaryKeyParser.makeParser[Persisted[T]]


    Route.apply(DELETE, path, (req: HttpRequest.HttpRequest[Unit]) => {
      val keyStrings: List[String] =
        dynamicIndexes.map(req.path.segments.collect {
          case dynamic: Segment.Static => dynamic.segment.toString
        })
      val keyTuple: PrimaryKeyFields[Persisted[T]]#Out = parseKeys(keyStrings)

      toResponse(db.withConnection( x=> x.delete[Persisted[T]](keyTuple)))(enc.apply)

    }
    )


  inline def patch[P[_], T[_[_]] <: Product](entityName: String)(using // ← T now has correct kind
                                             db: Database,
                                             enc: BodyEncoder[P, Iterator[Persisted[T]]],
                                             sw: SocketWriter[P],
                                             ch: ConnectionHandler[P],
                                             ec: ExecutionContext,
                                             m: Mirror.ProductOf[Updated[T]],
                                             pm: Mirror.ProductOf[Persisted[T]],
                                            ): Route[P, PATCH, Updated[T], Iterator[Persisted[T]]] =

    val primaryKeys = PrimaryKeyExtractor.getPrimaryKey[Persisted[T]].map(x => s"{$x}").mkString("/")
    val path = s"/api/$entityName/$primaryKeys".toPath
    val dynamicIndexes = path.segments.zipWithIndex.collect {
      case (dynamic: Segment.Dynamic, index) => index
    }
    val parseKeys = PrimaryKeyParser.makeParser[Updated[T]]


    given dial: SqlDialect = db.dial

    Route.apply(PATCH, path, (req: HttpRequest.HttpRequest[Updated[T]]) => {

      
      val keyStrings: List[String] =
        dynamicIndexes.map(req.path.segments.collect {
          case dynamic: Segment.Static => dynamic.segment.toString
        })
      val keyTuple: PrimaryKeyFields[Updated[T]]#Out = parseKeys(keyStrings)

      toResponse(db.withConnection( x=> x.update[Updated[T], Persisted[T]](req.body,keyTuple)))(enc.apply)

    }
    )

  inline def resourceRoutes[P[_], T[_[_]] <: Product](entityName: String)(using
                                                      // Common requirements
                                                      db: Database,
                                                      enc: BodyEncoder[P, Iterator[Persisted[T]]],
                                                      sw: SocketWriter[P],
                                                      ch: ConnectionHandler[P],
                                                      ec: ExecutionContext,
                                                      mPersisted: Mirror.ProductOf[Persisted[T]],
                                                      mNew: Mirror.ProductOf[New[T]],
                                                      jdsNew: JsonDeserializer[New[T]],
                                                      jsdUpdated: JsonDeserializer[Updated[T]],
                                                      mUpdated: Mirror.ProductOf[Updated[T]],
                                                                          row: Row[Persisted[T]],
                                                                          sqlo: SqlSelect[Optional[T]],
                                                                          sqlInsert: SqlInsert[New[T]],
                                                                          sqlDelete: SqlDelete[Persisted[T]],
                                                                          fb: FieldBinder[Optional[T]],
                                                      queryDeserializer: QueryDeserializer[Optional[T]]
                                                     ): List[Route[P, ? <: HttpMethod,?, ?]] = {

    List(
      get[P, T](entityName), // GET /api/entity
      getWhere[P, T](entityName), // GET /api/entity/{id}
      post[P, T](entityName), // POST /api/entity
      delete[P, T](entityName), // DELETE /api/entity/{id}
      patch[P, T](entityName) // PATCH /api/entity/{id}
    )
  }


  extension [P[_] <: Protocal[?], M <: HttpMethod, Req, Res](routes: List[Route[P, M, Req, Res]])
    inline def lazily: List[Lazy[Route[P, M, Req,Res]]] =
      routes.map(r => Lazy( () =>  r))

  extension [R <: Route[?, ?, ?, ?]](routes: List[R])
    inline def lazily: List[Lazy[R]] =
      routes.map(r => new Lazy(() => r))
