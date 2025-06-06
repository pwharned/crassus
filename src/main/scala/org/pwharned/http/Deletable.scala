package org.pwharned.http

import org.pwharned.database.HKD.{Id, New, Persisted}
import org.pwharned.database.{Database, PrimaryKeyExtractor, PrimaryKeyFields, SqlDelete, delete}
import org.pwharned.http.HttpMethod.{DELETE, GET, HttpMethod, POST}
import org.pwharned.http.{Headers, HttpRequest, HttpResponse, Segment}
import org.pwharned.json.JsonSerializer
import org.pwharned.route.Router.{Route, route}
import org.pwharned.macros.toTuple
import org.pwharned.route.{ConnectionHandler, Http, SocketWriter}

import java.nio.charset.StandardCharsets
import scala.compiletime.constValue
import scala.concurrent.{ExecutionContext, Future}
import scala.deriving.Mirror
import scala.util.{Failure, Success}


trait Deletable[T]:
  def delete(using ec: ExecutionContext): Route[Http, HttpMethod] // renamed to delete for clarity

object Deletable:
  inline given derive[T[F[_]] <: Product](using
                                          m: Mirror.ProductOf[T[Id]], 
                                          sqlDelete: SqlDelete[T[Id]], 
                                          serializer: JsonSerializer[T[Id]],
                                         socketWriter: SocketWriter[Http],
                                          connectionHandler: ConnectionHandler[Http]
                                         ): Deletable[T[Id]] = new Deletable[T[Id]]:
    def delete(using ec: ExecutionContext): Route[Http, HttpMethod] =
      val tableName = constValue[m.MirroredLabel]
      // Use PrimaryKeyExtractor on T[Id], which is your Persisted[T]
      val primaryKeys = PrimaryKeyExtractor.getPrimaryKey[T[Id]].map(x => s"{$x}").mkString("/")
      val path = s"/api/$tableName/$primaryKeys".toPath
      val dynamicIndexes = path.segments.zipWithIndex.collect {
        case (dynamic: Segment.Dynamic, index) => index
      }

      route(DELETE, path, (req: HttpRequest.HttpRequest) => {
        val keyStrings: List[String] =
          dynamicIndexes.map(req.path.segments.collect {
            case dynamic: Segment.Static => dynamic.segment.toString
          })

        // If PrimaryKeyFields is defined for the persisted type, make sure the type uses T[Id]
        val b: PrimaryKeyFields[T[Id]]#Out =
          toTuple(keyStrings).asInstanceOf[PrimaryKeyFields[T[Id]]#Out]

        handleTransaction{
          Database.delete[T[Id]](b)
        }
      })



