package org.pwharned.macros

import generated.user
import org.pwharned.database.Database
import org.pwharned.http.HttpMethod.{GET, HttpMethod, POST}
import org.pwharned.http.{HttpRequest, HttpResponse}
import org.pwharned.route.Router.{Route, route}

import java.nio.charset.StandardCharsets
import scala.compiletime.constValue
import scala.concurrent.{ExecutionContext, Future}
import scala.deriving.Mirror


object RouteRegistry:
  def getRoutes[T <: Product](using Routable[T], ExecutionContext): List[Route[HttpMethod]] =
    summon[Routable[T]].allRoutes


trait Routable[T]:
  def get(using ec: ExecutionContext): Route[HttpMethod]
  def post(using ec: ExecutionContext): Route[HttpMethod]
  def allRoutes(using ec: ExecutionContext): List[Route[HttpMethod]] = List(get, post)

  
  


object Routable:
  inline given derive[T <: Product](using m: Mirror.ProductOf[T]): Routable[T] =
    new Routable[T]:
      def get(using ec: ExecutionContext): Route[HttpMethod]  =
        val tableName = constValue[m.MirroredLabel]
        route(GET, s"/api/$tableName".asPath,(req: HttpRequest.HttpRequest) => {
          Database.retrieve[T]
        }   )

      def post(using ec: ExecutionContext): Route[HttpMethod] =
        val tableName = constValue[m.MirroredLabel]
        route(POST, s"/api/$tableName".asPath, (req: HttpRequest.HttpRequest) => {


          val bytes = new Array[Byte](req.body.remaining())
          req.body.get(bytes)
          // Decode the bytes using the desired charset.
          val s = new String(bytes, StandardCharsets.UTF_8)
          s.deserialize[T] match
            case Left(value) =>  Future(HttpResponse.error(value.message))
            case Right(value) => Database.create[T](value)
         
        })

      def getBY(using ec: ExecutionContext): Route[HttpMethod] =
        val tableName = constValue[m.MirroredLabel]
        route(POST, s"/api/$tableName".asPath, (req: HttpRequest.HttpRequest) => {


          val bytes = new Array[Byte](req.body.remaining())
          req.body.get(bytes)
          // Decode the bytes using the desired charset.
          val s = new String(bytes, StandardCharsets.UTF_8)
          s.deserialize[T] match
            case Left(value) => Future(HttpResponse.error(value.message))
            case Right(value) => Database.create[T](value)

        })





