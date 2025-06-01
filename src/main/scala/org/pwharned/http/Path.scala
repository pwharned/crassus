package org.pwharned.http
import scala.language.implicitConversions

// Define HttpPath as an opaque type with String as its underlying representation.

object HttpPath:
  opaque type HttpPath = List[IdentifierOrSegment]
  opaque type IdentifierOrSegment = Identifier.Identifier | Segment.Segment
  inline def apply(p: String): HttpPath =
    p.stripPrefix("/").split('/').filter(_.nonEmpty).toList.map { s =>

      Identifier.fromString(Segment(s)) match
        case Some(value) => value
        case None => Segment(s)

    }

  extension (id: HttpPath)
  // Extract the value inside the brackets.

    def asList: List[IdentifierOrSegment] = id


object Identifier:
  opaque type Identifier = Segment.Segment
  inline def fromString(raw: Segment.Segment): Option[Identifier] =
    if raw.startsWith("{") && raw.endsWith("}") then Some(Identifier(raw)) else None
  inline def apply(raw: Segment.Segment): Identifier = raw


  extension (id: Identifier)
    // Extract the value inside the brackets.
    inline def value: String = id.value.substring(1, id.value.length - 1)

object Segment:
  opaque type Segment = String

  inline def apply(raw: String): Segment = raw

  extension (seg: Segment)
    inline def startsWith(str: String) = seg.startsWith(str)
    inline def endsWith(str: String) = seg.endsWith(str)
    inline def substring(a: Int, b: Int): Segment = seg.substring(a,b)
    inline def length = seg.length






// Use the runtime classifier on each segment:
