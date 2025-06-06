package org.pwharned.http

import org.pwharned.database.HKD.*
import org.pwharned.database.{Database, PrimaryKeyExtractor, PrimaryKeyFields, SqlSelect, retrieve, retrieveParameterized}
import org.pwharned.http.HttpMethod.{GET, HttpMethod}
import org.pwharned.http.{Headers, HttpRequest, HttpResponse}
import org.pwharned.json.{JsonSerializer, serialize}
import org.pwharned.macros.toTuple
import org.pwharned.route.Router.{Route, route}
import org.pwharned.parse.{QueryDeserializer, fromQuery}

import scala.compiletime.constValue
import scala.concurrent.ExecutionContext
import scala.deriving.Mirror
import scala.util.{Failure, Success}


trait Retrievable[T]:
  def get(using ec: ExecutionContext): Route[HttpMethod]
  def getWhere(using ec: ExecutionContext): Route[HttpMethod]



object Retrievable:
  inline given derive[T[F[_]] <: Product](using m: Mirror.ProductOf[Persisted[T]], m2: Mirror.ProductOf[T[Id]], sqlSelect: SqlSelect[T[Id]],sqlSelectOptional: SqlSelect[Optional[T]], queryDeserializer: QueryDeserializer[Optional[T]], serializer: JsonSerializer[T[Id]],optionalSerializaer: JsonSerializer[Optional[T]]): Retrievable[T[Id]] =
    new Retrievable[T[Id]]:
      def get(using ec: ExecutionContext): Route[HttpMethod]  =
        val tableName = constValue[m.MirroredLabel]
        route(GET, s"/api/$tableName".toPath,(req: HttpRequest.HttpRequest) => {

          val queryString = req.path.query.value
          queryString.stripMargin.fromQuery[Optional[T]] match {
            case Left(value)  =>
              Database.retrieve[T].map( x => x.map(y => y.serialize) match {
                case Failure(exception) => HttpResponse.error(exception.toString)
                case Success(value) => HttpResponse.ok(value,Headers.apply(Map("content-type" -> "Application/json")))
              }      )
            case Right(value) =>
              println(value)
              Database.retrieveParameterized[Optional[T]](value).map(x => x.map(y => y.serialize) match {
                case Failure(exception) => HttpResponse.error(exception.toString)
                case Success(value) => HttpResponse.ok(value,Headers.apply(Map("content-type" -> "Application/json")))
              }      )
          
          
          }

        }


        )

      def getWhere(using ec: ExecutionContext): Route[HttpMethod] =
        val tableName = constValue[m.MirroredLabel]
        // Use PrimaryKeyExtractor on T[Id], which is your Persisted[T]
        val primaryKeys = PrimaryKeyExtractor.getPrimaryKey[T[Id]].map(x => s"{$x}").mkString("/")
        val path = s"/api/$tableName/$primaryKeys".toPath
        val dynamicIndexes = path.segments.zipWithIndex.collect {
          case (dynamic: Segment.Dynamic, index) => index
        }

        route(GET, path, (req: HttpRequest.HttpRequest) => {
          val keyStrings: List[String] =
            dynamicIndexes.map(req.path.segments.collect {
              case dynamic: Segment.Static => dynamic.segment.toString
            })

          // If PrimaryKeyFields is defined for the persisted type, make sure the type uses T[Id]
          val b: PrimaryKeyFields[T[Id]]#Out =
            toTuple(keyStrings).asInstanceOf[PrimaryKeyFields[T[Id]]#Out]

          handleTransaction {
            Database.retrieve[T[Id]](b)
          }
        })

