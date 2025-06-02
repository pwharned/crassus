package org.pwharned.macros

import generated.user
import org.pwharned.database.Database
import org.pwharned.http.HttpMethod.{DELETE, GET, HttpMethod, POST}
import org.pwharned.http.{HttpRequest, HttpResponse, Segment}
import org.pwharned.route.Router.{Route, route}

import scala.compiletime.summonInline
import java.nio.charset.StandardCharsets
import scala.compiletime.constValue
import scala.concurrent.{ExecutionContext, Future}
import scala.deriving.Mirror
import generated.PrimaryKey
import org.pwharned.http.Identifier.Identifier
import org.pwharned.http.PathSegment.PathSegment

import scala.compiletime.{erasedValue, summonInline}
import scala.util.Try


trait FromString[A]:
  def parse(s: String): A

object FromString:
  given FromString[Int] with
    def parse(s: String): Int = s.toInt

  given FromString[String] with
    def parse(s: String): String = s

inline def listToTuple[T <: Tuple](list: List[String]): T = {
  inline erasedValue[T] match
    case _: EmptyTuple =>
      println(list)
      EmptyTuple.asInstanceOf[T]
    case _: (h *: t) =>
      println("hello from " + list.head)
      // Convert the head string to type h
      val head: h = summonInline[FromString[h]].parse(list.head)
      
      // Recursively convert the remainder of the list to type t
      val tail: t = listToTuple[t](list.tail)
      (head *: tail).asInstanceOf[T]
}

def toTuple(list: List[String]): Tuple = list match {
  case head :: tail =>
    // Try to parse the head element into an Int,
    // using summonInline to obtain the given FromString[Int]
    val maybeInt: Option[Int] =
      Try { summonInline[FromString[Int]].parse(head) }.toOption

    maybeInt match {
      case Some(value) =>
        // Prepend the parsed Int to the tuple produced by the tail.
        value *: toTuple(tail)
      case None =>
        // If the head cannot be parsed, throw an exception.
        summonInline[FromString[Int]].parse(head) *: toTuple(tail)
    }
  case Nil =>
    // Base case: an empty list corresponds to an empty tuple.
    EmptyTuple
}


object RouteRegistry:
  def getRoutes[T <: Product](using Routable[T], ExecutionContext): List[Route[HttpMethod]] =
    summon[Routable[T]].allRoutes


trait Routable[T]:
  def get(using ec: ExecutionContext): Route[HttpMethod]
  def post(using ec: ExecutionContext): Route[HttpMethod]

  def delete(using ec: ExecutionContext): Route[HttpMethod]

  def allRoutes(using ec: ExecutionContext): List[Route[HttpMethod]] = List(get, post,delete)

  
  


object Routable:
  inline given derive[T <: Product](using m: Mirror.ProductOf[T]): Routable[T] =
    new Routable[T]:
      def get(using ec: ExecutionContext): Route[HttpMethod]  =
        val tableName = constValue[m.MirroredLabel]
        route(GET, s"/api/$tableName".toPath,(req: HttpRequest.HttpRequest) => {
          Database.retrieve[T]
        }   )

      def post(using ec: ExecutionContext): Route[HttpMethod] =
        val tableName = constValue[m.MirroredLabel]
        route(POST, s"/api/$tableName".toPath, (req: HttpRequest.HttpRequest) => {


          val bytes = new Array[Byte](req.body.remaining())
          req.body.get(bytes)
          // Decode the bytes using the desired charset.
          val s = new String(bytes, StandardCharsets.UTF_8)
          s.deserialize[T] match
            case Left(value) =>  Future(HttpResponse.error(value.message))
            case Right(value) => Database.create[T](value)
         
        })

      def delete(using ec: ExecutionContext): Route[HttpMethod] =
        val tableName = constValue[m.MirroredLabel]
        val primaryKeys = PrimaryKeyExtractor.getPrimaryKey[T].map(x => s"{$x}").mkString("/")
        val path = s"/api/$tableName/$primaryKeys".toPath
        val dynamicIndexes = path.segments.zipWithIndex.collect {
          case (dynamic: Segment.Dynamic, index) => index
        }

        route(DELETE, path, (req: HttpRequest.HttpRequest) => {
          val expectedSize = PrimaryKeyExtractor.getPrimaryKey[T].size


          val keyStrings:List[String] = dynamicIndexes.map(req.path.toPath.segments.collect {
            case dynamic: Segment.Static => dynamic.segment.toString
          })

          val b: PrimaryKeyFields[T]#Out = toTuple(keyStrings).asInstanceOf[PrimaryKeyFields[T]#Out]
          

          Database.delete[T](b)


        })





