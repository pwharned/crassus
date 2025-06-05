package org.pwharned.http

import org.pwharned.database.HKD.*
import org.pwharned.database.*
import org.pwharned.http.HttpMethod.{HttpMethod, PATCH}
import org.pwharned.http.{HttpRequest, HttpResponse, Segment}
import org.pwharned.json.{JsonSerializer, deserialize}
import org.pwharned.macros.toTuple
import org.pwharned.route.{ConnectionHandler, Http, SocketWriter}
import org.pwharned.route.Router.{Route, given_Conversion_Route_Route, route}

import java.nio.charset.StandardCharsets
import scala.compiletime.constValue
import scala.concurrent.{ExecutionContext, Future}
import scala.deriving.Mirror


trait Updatable[T]:
  def update(using ec: ExecutionContext): Route[Http,HttpMethod] // renamed to delete for clarity

object Updatable:
  inline given derive[T[F[_]] <: Product](using
                                          m: Mirror.ProductOf[Updated[T]], // Use T[Id], the persisted type
                                          sqlDelete: SqlUpdate[Updated[T]], // Must also work on T[Id]
                                          serializer: JsonSerializer[Updated[T]], // Likewise for the serializer
                                         connectionHandler: ConnectionHandler[Http],
                                         socketWriter: SocketWriter[Http]
                                         ): Updatable[Updated[T]] = 
    new Updatable[Updated[T]]:
        
      def update(using ec: ExecutionContext): Route[Http, HttpMethod] =
        val tableName = constValue[m.MirroredLabel]
        val primaryKeys = PrimaryKeyExtractor.getPrimaryKey[Updated[T]].map(x => s"{$x}").mkString("/")
        val path = s"/api/$tableName/$primaryKeys".toPath
        val dynamicIndexes = path.segments.zipWithIndex.collect {
          case (dynamic: Segment.Dynamic, index) => index
        }

        route(PATCH, path, (req: HttpRequest.HttpRequest) => {


          val keyStrings: List[String] = dynamicIndexes.map(req.path.segments.collect {
            case dynamic: Segment.Static => dynamic.segment.toString
          })

          val b: PrimaryKeyFields[Updated[T]]#Out = toTuple(keyStrings).asInstanceOf[PrimaryKeyFields[Updated[T]]#Out]


          val bytes = new Array[Byte](req.body.remaining())
          req.body.get(bytes)
          // Decode the bytes using the desired charset.
          val s = new String(bytes, StandardCharsets.UTF_8)
          s.deserialize[Updated[T]] match
            case Left(value) =>  Future(HttpResponse.error(value.message))
            case Right(value) => handleTransaction{
              Database.update[Updated[T]](value,b)
            }




        })





