package org.pwharned.route

import org.pwharned.`lazy`.Lazy
import org.pwharned.database.{Database, FieldBinder, PrimaryKeyExtractor, PrimaryKeyFields, SqlInsert, SqlSelect, create, delete, retrieve, retrieveParameterized, update}
import org.pwharned.database.HKD.*
import org.pwharned.http.HttpMethod.{DELETE, GET, HttpMethod, PATCH, POST}
import org.pwharned.http.{BodyEncoder, HttpRequest, HttpResponse, Protocal, Segment, SocketWriter, toPath}
import org.pwharned.route.Router.Route
import org.pwharned.json.{JsonDeserializer, JsonSerializer, deserialize}
import org.pwharned.macros.toTuple
import org.pwharned.parse.{QueryDeserializer, fromQuery}
import org.pwharned.openapi.{Schema, given_Schema_Iterator}
import org.pwharned.openapi.given_Schema_Unit

import java.nio.charset.StandardCharsets
import scala.concurrent.Future
import scala.compiletime.{constValue, summonInline}
import scala.concurrent.ExecutionContext
import scala.deriving.Mirror
import scala.util.{Failure, Success, Try}

private def toResponse[A](fa: Future[Try[A]])
                         (encode: A => HttpResponse[A])
                         (using ExecutionContext): Future[HttpResponse[A]  ]  =

  fa.map {
    case Failure(exception) => HttpResponse.error(exception.getMessage)
    case Success(value) => encode(value)
  }
object RouteRegistry:

  inline def get[P[_], T[_[_]]<: Product](using // ← T now has correct kind
                                         db: Database.type,
                                         sql: SqlSelect[Persisted[T]], // SqlSelect[UserPersisted]
                                         sqls: SqlSelect[Optional[T]],
                                           enc: BodyEncoder[P, Iterator[Persisted[T]]],
                                         sw: SocketWriter[P],
                                           queryDeserializer: QueryDeserializer[Optional[T]],
                                         ch: ConnectionHandler[P],
                                         ec: ExecutionContext,
                                         m: Mirror.ProductOf[Persisted[T]]
                                        ): Route[P, GET,  Unit, Iterator[Persisted[T]]] =

    val table = constValue[m.MirroredLabel]


    route(GET, s"/api/$table".toPath, (req: HttpRequest.HttpRequest[Unit]) =>

      {

        val queryString = req.path.query.value

        val q = queryString.stripMargin.fromQuery[Optional[T]]
        q match {
          case Left(value) =>  toResponse(db.retrieve[Persisted[T]])(enc.apply)
          case Right(value) => toResponse(db.retrieveParameterized[Optional[T], Persisted[T]](value))(enc.apply)


        }
      }

    )

  inline def getWhere[P[_], T[_[_]] <: Product](using // ← T now has correct kind
                                            db: Database.type,
                                            sql: SqlSelect[Persisted[T]], // SqlSelect[UserPersisted]
                                            enc: BodyEncoder[P, Iterator[Persisted[T]]],
                                            sw: SocketWriter[P],
                                            ch: ConnectionHandler[P],
                                            ec: ExecutionContext,
                                            m: Mirror.ProductOf[Persisted[T]]
                                           ): Route[P, GET, Unit, Iterator[Persisted[T]]] =

    val table = constValue[m.MirroredLabel]

    // Use PrimaryKeyExtractor on T[Id], which is your Persisted[T]
    val primaryKeys = PrimaryKeyExtractor.getPrimaryKey[Persisted[T]].map(x => s"{$x}").mkString("/")
    val path = s"/api/$table/$primaryKeys".toPath
    val dynamicIndexes = path.segments.zipWithIndex.collect {
      case (dynamic: Segment.Dynamic, index) => index
    }
    route(GET, path, (req: HttpRequest.HttpRequest[Unit]) =>
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

  inline def post[P[_], T[_[_]] <: Product](using // ← T now has correct kind
                                                 db: Database.type,
                                                 sql: SqlSelect[Persisted[T]], // SqlSelect[UserPersisted]
                                              sqlC: SqlInsert[New[T]],
                                              enc: BodyEncoder[P, Iterator[Persisted[T]]],
                                                 sw: SocketWriter[P],
                                                 ch: ConnectionHandler[P],
                                                 ec: ExecutionContext,
                                            fb: FieldBinder[New[T]],
                                              jds: JsonDeserializer[New[T]],
                                                 m: Mirror.ProductOf[Persisted[T]],
                                            mr: Mirror.ProductOf[New[T]]
                                                ): Route[P, POST, New[T], Iterator[Persisted[T]]] =

    val table = constValue[m.MirroredLabel]
    // Use PrimaryKeyExtractor on T[Id], which is your Persisted[T]
    val path = s"/api/$table".toPath

    route(POST, path, (req: HttpRequest.HttpRequest[New[T]]) => {
    
     toResponse(db.create[New[T], Persisted[T]](req.body))(enc.apply)


    }
    )

  inline def delete[P[_], T[_[_]] <: Product](using // ← T now has correct kind
                                              db: Database.type,
                                              sql: SqlSelect[Persisted[T]], // SqlSelect[UserPersisted]
                                              sqlC: SqlInsert[New[T]],
                                              enc: BodyEncoder[P, Iterator[Persisted[T]]],
                                              sw: SocketWriter[P],
                                              ch: ConnectionHandler[P],
                                              ec: ExecutionContext,
                                              m: Mirror.ProductOf[Persisted[T]]
                                             ): Route[P, DELETE, Unit, Iterator[Persisted[T]]] =
  
    val tableName = constValue[m.MirroredLabel]
    // Use PrimaryKeyExtractor on T[Id], which is your Persisted[T]
    val primaryKeys = PrimaryKeyExtractor.getPrimaryKey[Persisted[T]].map(x => s"{$x}").mkString("/")
    val path = s"/api/$tableName/$primaryKeys".toPath
    val dynamicIndexes = path.segments.zipWithIndex.collect {
      case (dynamic: Segment.Dynamic, index) => index
    }
  
    route(DELETE, path, (req: HttpRequest.HttpRequest[Unit]) => {
      val keyStrings: List[String] =
        dynamicIndexes.map(req.path.segments.collect {
          case dynamic: Segment.Static => dynamic.segment.toString
        })
      // If PrimaryKeyFields is defined for the persisted type, make sure the type uses T[Id]
      val b: PrimaryKeyFields[Persisted[T]]#Out =
        toTuple(keyStrings).asInstanceOf[PrimaryKeyFields[Persisted[T]]#Out]

      toResponse(db.delete[Persisted[T]](b))(enc.apply)


    }
    )


  inline def patch[P[_], T[_[_]] <: Product](using // ← T now has correct kind
                                              db: Database.type,
                                              sql: SqlSelect[Persisted[T]], // SqlSelect[UserPersisted]
                                              sqlC: SqlInsert[New[T]],
                                              enc: BodyEncoder[P, Iterator[Persisted[T]]],
                                              jsd: JsonDeserializer[Updated[T]],
                                              sw: SocketWriter[P],
                                              ch: ConnectionHandler[P],
                                              ec: ExecutionContext,
                                              m: Mirror.ProductOf[Updated[T]],
                                             pm: Mirror.ProductOf[Persisted[T]]
                                            
                                             ): Route[P, PATCH, Updated[T], Iterator[Persisted[T]]] =
  
    val tableName = constValue[m.MirroredLabel]
    // Use PrimaryKeyExtractor on T[Id], which is your Persisted[T]
    val primaryKeys = PrimaryKeyExtractor.getPrimaryKey[Updated[T]].map(x => s"{$x}").mkString("/")
    val path = s"/api/$tableName/$primaryKeys".toPath
    val dynamicIndexes = path.segments.zipWithIndex.collect {
      case (dynamic: Segment.Dynamic, index) => index
    }
  
    route(PATCH, path, (req: HttpRequest.HttpRequest[Updated[T]]) => {
  
      val keyStrings: List[String] = dynamicIndexes.map(req.path.segments.collect {
        case dynamic: Segment.Static => dynamic.segment.toString
      })

      val b: PrimaryKeyFields[Updated[T]]#Out = toTuple(keyStrings).asInstanceOf[PrimaryKeyFields[Updated[T]]#Out]


         toResponse(db.update[Updated[T], Persisted[T]](req.body,b))(enc.apply)
  
    }
    )

  inline def resourceRoutes[P[_], T[_[_]] <: Product](using
                                                      // Common requirements
                                                      db: Database.type,
                                                      sql: SqlSelect[Persisted[T]],
                                                      sqls: SqlSelect[Optional[T]],
                                                      enc: BodyEncoder[P, Iterator[Persisted[T]]],
                                                      sw: SocketWriter[P],
                                                      ch: ConnectionHandler[P],
                                                      ec: ExecutionContext,
                                                      mPersisted: Mirror.ProductOf[Persisted[T]],
                                                      mNew: Mirror.ProductOf[New[T]],
                                                      // Create-specific requirements
                                                      sqlC: SqlInsert[New[T]],
                                                      jdsNew: JsonDeserializer[New[T]],
                                                      // Update-specific requirements
                                                      jsdUpdated: JsonDeserializer[Updated[T]],
                                                      mUpdated: Mirror.ProductOf[Updated[T]],

                                                      // Query-specific requirements
                                                      queryDeserializer: QueryDeserializer[Optional[T]]
                                                     ): List[Route[P, ? <: HttpMethod,?, ?]] = {

    given fb: FieldBinder[New[T]] = summonInline[FieldBinder[New[T]]]
    List(
      get[P, T], // GET /api/entity
      getWhere[P, T], // GET /api/entity/{id}
      post[P, T], // POST /api/entity
      delete[P, T], // DELETE /api/entity/{id}
      patch[P, T] // PATCH /api/entity/{id}
    )
  }


  extension [P[_] <: Protocal[_], M <: HttpMethod, Req, Res](routes: List[Route[P, M, Req, Res]])
    inline def lazily: List[Lazy[Route[P, M, Req,Res]]] =
      routes.map(r => Lazy( () =>  r))

  extension [R <: Route[_, _, _, _]](routes: List[R])
    inline def lazily: List[Lazy[R]] =
      routes.map(r => new Lazy(() => r))
