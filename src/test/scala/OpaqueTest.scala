import scala.reflect.TypeTest

object Identifier:
  opaque type Identifier = String
  def fromString(x: String): Option[x.type & Identifier] =
    Option.when(x.head == '{' && x.last == '}')(x)


object Segment:
  opaque type Segment = String
  def fromString(x: String): Option[x.type & Segment] =
    Option(x)
  inline def apply(raw: String): Segment = raw

  extension (seg: Segment)
    inline def toString: String = seg
    inline def startsWith(str: String): Boolean = seg.startsWith(str)
    inline def endsWith(str: String): Boolean = seg.endsWith(str)
    inline def substring(a: Int, b: Int): Segment = seg.substring(a, b)
    inline def length: Int = seg.length
    def head: Char = seg.head
    def last: Char = seg.last
val notId: String = "12345"
val isId: String = "{abc}"


@main 
def p(): Unit =
    given TypeTest[String, Identifier.Identifier] = (x) => Identifier.fromString(x)
    given TypeTest[String, Segment.Segment] = (x) => Segment.fromString(x)

    val x = notId match {
      case x: Identifier.Identifier => "is id"
      case x: Segment.Segment => "is segment"

      case _ => "not id"
    }
    val y = isId match {
      case x: Identifier.Identifier => "is id"
      case x: Segment.Segment => "is segment"

      case _ => "not id"
    }
    println (s"$x; $y")
  
