package org.pwharned.http

import org.pwharned.database.HKD.{Id, New, Persisted}
import org.pwharned.database.{Database, SqlSelect, create}
import org.pwharned.http.HttpMethod.{GET, HttpMethod, POST}
import org.pwharned.http.{Headers, HttpRequest, HttpResponse}
import org.pwharned.json.{JsonSerializer, deserialize}
import org.pwharned.route.Router.Route
import org.pwharned.route.{Http, route}
import org.pwharned.route.{httpWriter, httpConnection}
import java.nio.charset.StandardCharsets
import scala.compiletime.constValue
import scala.concurrent.{ExecutionContext, Future}
import scala.deriving.Mirror
import scala.util.{Failure, Success}


trait Creatable[T]:
  def post(using ec: ExecutionContext): Route[Http,HttpMethod]



object Creatable:
  inline given derive[T[F[_]] <: Product](using m: Mirror.ProductOf[New[T]], sqlSelect: SqlSelect[T[Id]], serializer: JsonSerializer[Persisted[T]]): Creatable[Persisted[T]] =
    new Creatable[Persisted[T]]:
      def post(using ec: ExecutionContext): Route[Http,HttpMethod] =
        val tableName = constValue[m.MirroredLabel]
        route(POST, s"/api/$tableName".toPath, (req: HttpRequest.HttpRequest) => {


          val bytes = new Array[Byte](req.body.remaining())
          req.body.get(bytes)
          // Decode the bytes using the desired charset.
          val s = new String(bytes, StandardCharsets.UTF_8)
          s.deserialize[New[T]] match
            case Left(value) => Future(HttpResponse.error(value.message))
            case Right(value) => {
              handleTransaction{
                Database.create[New[T]](value)
              }
            }

        })
