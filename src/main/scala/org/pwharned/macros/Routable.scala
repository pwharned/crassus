package org.pwharned.macros

import generated.user
import org.pwharned.database.Database
import org.pwharned.http.HttpMethod.{DELETE, GET, HttpMethod, POST}
import org.pwharned.http.{HttpRequest, HttpResponse, Segment}
import org.pwharned.route.Router.{Route, route}

import java.nio.charset.StandardCharsets
import scala.compiletime.constValue
import scala.concurrent.{ExecutionContext, Future}
import scala.deriving.Mirror
import generated.PrimaryKey
import org.pwharned.http.Identifier.Identifier
import org.pwharned.http.PathSegment.PathSegment


object RouteRegistry:
  def getRoutes[T <: Product](using Routable[T], ExecutionContext,KeyTupleBuilder[PrimaryKeyFields[T]#Out]): List[Route[HttpMethod]] =
    summon[Routable[T]].allRoutes


trait Routable[T]:
  def get(using ec: ExecutionContext): Route[HttpMethod]
  def post(using ec: ExecutionContext): Route[HttpMethod]

  def delete(using ec: ExecutionContext,builder: KeyTupleBuilder[PrimaryKeyFields[T]#Out]): Route[HttpMethod]

  def allRoutes(using ec: ExecutionContext,builder: KeyTupleBuilder[PrimaryKeyFields[T]#Out]): List[Route[HttpMethod]] = List(get, post,delete)

  
  


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

      def delete(using ec: ExecutionContext, builder: KeyTupleBuilder[PrimaryKeyFields[T]#Out]): Route[HttpMethod] =
        val tableName = constValue[m.MirroredLabel]
        val primaryKeys = PrimaryKeyExtractor.getPrimaryKey[T].mkString("/")
        
        route(DELETE, s"/api/$tableName/$primaryKeys".asPath, (req: HttpRequest.HttpRequest) => {
          val keyStrings: List[String] = req.path.toPath.segments.collect {
            case dynamic: Segment.Dynamic => dynamic.segment.toString
          }
          // Use the recursive type class to build the expected tuple.
          val pkeys: PrimaryKeyFields[T]#Out = builder.build(keyStrings) match
            case Some(tuple) => tuple
            case None =>
              throw new IllegalArgumentException(
                s"Expected ${PrimaryKeyExtractor.getPrimaryKey[T].size} key(s), but found ${keyStrings.size}"
              )
              
            Database.delete[T](pkeys)


        })





