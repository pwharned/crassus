package org.pwharned.http

import scala.annotation.tailrec


// Define HttpPath as an opaque type with String as its underlying representation.
opaque type Segment = String
final case class Identifier(value: Segment)
type IdentifierOrSegment = Identifier | Segment
opaque type HttpPath = List[IdentifierOrSegment]


object HttpPath:

  inline def apply(p: String): HttpPath =
    p.stripPrefix("/").split('/').filter(_.nonEmpty).toList.map { s =>

      Identifier.fromString(s) match
        case Some(value) => value
        case None => Segment(s)

    }

  extension (id: HttpPath)
    // Extract the value inside the brackets.
    
    def asList: List[IdentifierOrSegment] = id


extension (id: Identifier)
  // Extract the value inside the brackets.
  inline def value: String = id.value.substring(1, id.value.length - 1)

object Identifier:

  inline def fromString(raw: Segment): Option[Identifier] =
    if raw.startsWith("{") && raw.endsWith("}") then Some(apply(raw.substring(1, raw.length - 1))) else None


object Segment:
  inline def apply(raw: String): Segment = raw

extension (seg: Segment)
  inline def startsWith(str: String) = seg.startsWith(str)
  inline def endsWith(str: String) = seg.endsWith(str)
  inline def substring(a: Int, b: Int): Segment = seg.substring(a,b)
  inline def length = seg.length





@main def runPathSegmentDemo(): Unit =

  // Suppose we have an HTTP path string.
  inline val rawPath = "/api/resources/{abc123}/details"
  val path  = HttpPath(rawPath)



  // Use the runtime classifier on each segment:
  println(path)