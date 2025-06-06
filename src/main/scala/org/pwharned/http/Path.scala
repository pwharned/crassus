package org.pwharned.http
import org.pwharned.http.HttpPath.HttpPath

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.quoted.*

object PathSegment:
  opaque type PathSegment = String

  def apply(s: String): PathSegment = s
  extension (ps: PathSegment)
    def value: String = ps

object Query:
  opaque type Query = Option[String]
  given [T]: Conversion[T, Option[T]] = x => Some(x)
  val None: Query = Option.empty[String]
  def apply(s: String): Query = s

  extension (ps: Query)
    def value: String = ps.getOrElse("")

object Identifier:
  opaque type Identifier[X] = X

  def apply[X](x: X): Identifier[X] = x
  given [T]: Conversion[T, Identifier[T]] = x => Identifier(x)

sealed trait Segment
object Segment:
  final case class Static(segment: PathSegment.PathSegment) extends Segment
  final case class Dynamic(segment: Identifier.Identifier[PathSegment.PathSegment]) extends Segment

// The HttpPath is simply a List of Segments.
object HttpPath:
  opaque type HttpPath = (List[Segment], Query.Query)

  def apply(segments: List[Segment]): HttpPath = (segments, Query.None)
  def apply(segments: List[Segment], query: Query.Query): HttpPath = (segments, query)

  inline def apply(pathString: String): HttpPath =
    val url = pathString.split('?')
    val query = Query.apply(url.tail.mkString)
    
    val parts: List[Segment] = url.head
      .split("/")
      .filter(_.nonEmpty)
      .map { segment =>
        if segment.startsWith("{") && segment.endsWith("}") then
          val id = segment.substring(1, segment.length - 1)
          Segment.Dynamic(Identifier(toPathSegment(id)))
        else
          Segment.Static(toPathSegment(segment))
      }
      .toList

    HttpPath(parts, query)
  @tailrec
  def filter(f: HttpPath => HttpPath): HttpPath = HttpPath.filter(f)
  extension (hp: HttpPath)
    def segments: List[Segment] = hp._1
    def query: Query.Query = hp._2
  // Helper function to safely convert a String into a PathSegment.
  private def toPathSegment(s: String): PathSegment.PathSegment = PathSegment(s)

  // Inline method that lifts a compile-time literal into an HttpPath.
  inline def literal(inline s: String): HttpPath = ${ httpPathImpl('s) }

  private def httpPathImpl(s: Expr[String])(using Quotes): Expr[HttpPath] =
    import quotes.reflect.*
    s.value match
      case Some(pathString) =>
        val url = pathString.split('?')
        val query = url.tail.mkString
        val parts: List[Expr[Segment]] = url.head
          .split("/")
          .filter(_.nonEmpty)
          .toList
          .map { segment =>
            if segment.startsWith("{") && segment.endsWith("}") then
              // Extract the identifier without the braces.
              val id = segment.substring(1, segment.length - 1)
              // Now use our helper `toPathSegment` which is in scope.
              '{ Segment.Dynamic(Identifier(toPathSegment(${ Expr(id) }))) }
            else
              '{ Segment.Static(toPathSegment(${ Expr(segment) })) }
          }
        // Convert query part
        val queryExpr = '{ Query(${ Expr(query) }) }

        // Construct HttpPath
        '{ HttpPath(${ Expr.ofList(parts) }, $queryExpr) }
      case None =>
        quotes.reflect.report.error("HTTP path must be a compile-time constant.")
        '{ HttpPath(Nil, Query.None) }// At compile time, the literal will be converted to our HttpPath type.


extension (inline s: String) inline def asPath: HttpPath = HttpPath.literal(s)

extension ( s: String)  def toPath: HttpPath = HttpPath(s)

// Use the runtime classifier on each segment:
