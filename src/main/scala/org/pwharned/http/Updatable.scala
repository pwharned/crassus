package org.pwharned.http

import org.pwharned.database.HKD._
import org.pwharned.database.{Database, update, PrimaryKeyExtractor, PrimaryKeyFields, SqlDelete}
import org.pwharned.http.HttpMethod.{DELETE, GET, HttpMethod, PATCH, POST}
import org.pwharned.http.Identifier.Identifier
import org.pwharned.http.PathSegment.PathSegment
import org.pwharned.http.{Headers, HttpRequest, HttpResponse, Segment}
import org.pwharned.json.JsonSerializer
import org.pwharned.route.Router.{Route, route}
import org.pwharned.json.deserialize
import org.pwharned.macros.toTuple
import java.nio.charset.StandardCharsets
import scala.compiletime.{constValue, erasedValue, summonInline}
import scala.concurrent.{ExecutionContext, Future}
import scala.deriving.Mirror
import scala.util.{Failure, Success, Try}


trait Updatable[T]:
  def update(using ec: ExecutionContext): Route[HttpMethod] // renamed to delete for clarity

object Updatable:
  inline given derive[T[F[_]] <: Product](using
                                          m: Mirror.ProductOf[T[Id]], // Use T[Id], the persisted type
                                          sqlDelete: SqlDelete[T[Id]], // Must also work on T[Id]
                                          serializer: JsonSerializer[T[Id]] // Likewise for the serializer
                                         ): Updatable[T[Id]] = 
    new Updatable[T[Id]]:
        
      def update(using ec: ExecutionContext): Route[HttpMethod] =
        val tableName = constValue[m.MirroredLabel]
        val primaryKeys = PrimaryKeyExtractor.getPrimaryKey[T[Id]].map(x => s"{$x}").mkString("/")
        val path = s"/api/$tableName/$primaryKeys".toPath
        val dynamicIndexes = path.segments.zipWithIndex.collect {
          case (dynamic: Segment.Dynamic, index) => index
        }

        route(PATCH, path, (req: HttpRequest.HttpRequest) => {


          val keyStrings: List[String] = dynamicIndexes.map(req.path.toPath.segments.collect {
            case dynamic: Segment.Static => dynamic.segment.toString
          })

          val b: PrimaryKeyFields[T[Id]]#Out = toTuple(keyStrings).asInstanceOf[PrimaryKeyFields[T[Id]]#Out]


          val bytes = new Array[Byte](req.body.remaining())
          req.body.get(bytes)
          // Decode the bytes using the desired charset.
          val s = new String(bytes, StandardCharsets.UTF_8)
          s.deserialize[T[Id]] match
            case Left(value) =>  Future(HttpResponse.error(value.message))
            case Right(value) => Database.update[T[Id]](value)




        })





