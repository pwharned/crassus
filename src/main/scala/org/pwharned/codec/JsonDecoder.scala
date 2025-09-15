package org.pwharned.codec


import java.nio.charset.StandardCharsets
import scala.deriving.Mirror
import scala.compiletime.{constValueTuple, erasedValue, error, summonInline}
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions


// ─── 1. ByteDecoder Typeclass ─────────────────────────────────────────────────

trait JsonDecoder[T]:
  def decode(buf: Array[Byte], start: Int, end: Int): T

object JsonDecoder:
  import java.nio.charset.StandardCharsets
  import scala.deriving.*
  import scala.compiletime.*

  // ─── Primitive Instances ─────────────────────────────────────────────────────

  given JsonDecoder[Int] with
    inline def decode(buf: Array[Byte], start: Int, end: Int):  Int=
      var i = start
      var sign = 1
      if buf(i) == '-'.toByte then { sign = -1; i += 1 }
      var acc = 0
      while i < end do
        val b = buf(i)
        if b >= '0'.toByte && b <= '9'.toByte then
          acc = acc * 10 + (b - '0'.toByte)
          i += 1
        else throw Exception("Invalid integer")
      sign * acc

  given JsonDecoder[Long] with
    inline def decode(buf: Array[Byte], start: Int, end: Int): Long =
      var i = start
      var sign = 1
      if buf(i) == '-'.toByte then {
        sign = -1; i += 1
      }
      var acc: Long = 0
      while i < end do
        val b = buf(i)
        if b >= '0'.toByte && b <= '9'.toByte then
          acc = acc * 10 + (b - '0'.toByte)
          i += 1
        else throw Exception("Invalid integer")
      sign * acc

  given JsonDecoder[Double] with
    inline def decode(buf: Array[Byte], start: Int, end: Int): Double =
      var i = start
      var result = 0.0f
      var sign = 1.0f
      var decimalMultiplier = 0.1f
      var pastDecimal = false

      // Handle sign
      if i < end && buf(i) == '-'.toByte then {
        sign = -1.0f
        i += 1
      }

      while i < end do
        val b = buf(i)
        if b == '.'.toByte then
          if pastDecimal then throw new Exception("Multiple decimal points")
          pastDecimal = true
        else if b >= '0'.toByte && b <= '9'.toByte then
          val digit = (b - '0'.toByte).toFloat
          if pastDecimal then
            result += digit * decimalMultiplier
            decimalMultiplier *= 0.1f
          else
            result = result * 10.0f + digit
        else
          throw new Exception("Invalid float character")
        i += 1

      sign * result
  given JsonDecoder[Float] with
    inline def decode(buf: Array[Byte], start: Int, end: Int): Float =
      var i = start
      var result = 0.0f
      var sign = 1.0f
      var decimalMultiplier = 0.1f
      var pastDecimal = false

      // Handle sign
      if i < end && buf(i) == '-'.toByte then {
        sign = -1.0f
        i += 1
      }

      while i < end do
        val b = buf(i)
        if b == '.'.toByte then
          if pastDecimal then throw new Exception("Multiple decimal points")
          pastDecimal = true
        else if b >= '0'.toByte && b <= '9'.toByte then
          val digit = (b - '0'.toByte).toFloat
          if pastDecimal then
            result += digit * decimalMultiplier
            decimalMultiplier *= 0.1f
          else
            result = result * 10.0f + digit
        else
          throw new Exception("Invalid float character")
        i += 1

      sign * result

  given JsonDecoder[String] with
    inline def decode(buf: Array[Byte], start: Int, end: Int): String =
      if end - start >= 2 && buf(start) == '"'.toByte && buf(end - 1) == '"'.toByte then
        new String(buf, start + 1, end - start - 2, StandardCharsets.UTF_8)
      else
        throw new RuntimeException(s"Invalid string at [$start,$end)")

  given JsonDecoder[Boolean] with
    inline def decode(buf: Array[Byte], start: Int, end: Int): Boolean =
      val len = end - start
      if len == 4 then true
      else if len == 5 then false
      else
        throw new RuntimeException(s"Invalid boolean at ${start}: ${end}")

  // ─── Recursive-safe Option Decoder ───────────────────────────────────────────

  given optionDecoder[T](using dec: => JsonDecoder[T]): JsonDecoder[Option[T]] with
    inline def decode(buf: Array[Byte], start: Int, end: Int): Option[T] =
      val len = end - start
      if len == 4 &&
        buf(start) == 'n' &&
        buf(start + 1) == 'u' &&
        buf(start + 2) == 'l' &&
        buf(start + 3) == 'l' then None
      else Some(dec.decode(buf, start, end))

  // ─── Derivation ──────────────────────────────────────────────────────────────
  inline def isOptional[T]: Boolean =
    inline erasedValue[T] match
      case _: Option[?] => true
      case _ => false

  inline given derived[T](using m: Mirror.ProductOf[T]): JsonDecoder[T] =
    lazy val self: JsonDecoder[T] =
      val labels = getLabels[m.MirroredElemLabels]
      val decodersWithFlags = summonInstancesWithTypes[T, m.MirroredElemTypes](self)
      
      (buf: Array[Byte], start: Int, end: Int) =>
        val cursor = IntervalCursor(buf, start, end)
        val results = labels.zip(decodersWithFlags).map { case (name, (dec, isOpt)) =>
          cursor.extractField(name) match
            case Some((s, e)) =>
              dec.asInstanceOf[JsonDecoder[Any]].decode(buf, s, e)
            case None =>
              if isOpt then None
              else throw new RuntimeException("Missing field: $name")
        }

            m.fromProduct(Tuple.fromArray(results.toArray))


    self


  // ─── Utilities ───────────────────────────────────────────────────────────────

  inline def summonInstances[T, Elems <: Tuple](self: => JsonDecoder[T]): List[JsonDecoder[?]] =
    inline erasedValue[Elems] match
      case _: (elem *: elems) => deriveOrSummon[T, elem](self) :: summonInstances[T, elems](self)
      case _: EmptyTuple      => Nil

  inline def summonInstancesWithTypes[T, Elems <: Tuple](self: => JsonDecoder[T]): List[(JsonDecoder[?], Boolean)] =
    inline erasedValue[Elems] match
      case _: (elem *: elems) =>
        val dec = deriveOrSummon[T, elem](self)
        val opt = isOptional[elem]
        (dec, opt) :: summonInstancesWithTypes[T, elems](self)
      case _: EmptyTuple => Nil

  inline def deriveOrSummon[T, Elem](self: => JsonDecoder[T]): JsonDecoder[Elem] =
    inline erasedValue[Elem] match
      case _: T => self.asInstanceOf[JsonDecoder[Elem]]
      case _    => summonInline[JsonDecoder[Elem]]

  inline def getLabels[T <: Tuple]: List[String] =
    constValueTuple[T].toList.asInstanceOf[List[String]]



// ─── 4. IntervalCursor (with Nested-Object Slicing) ──────────────────────────


// ─── 5. Example: Nested Case Classes ──────────────────────────────────────────

@main def runNested(): Unit =

  case class Address(street: String, city: String)
  case class Person(
                     name: String,
                     age: Float,
                     active: Boolean,
                     address: Address,
                     spouse:Option[Person]
                   )

  type PersonRow = (name: String,age:Int)

  val json =
    """
      |{"name": "Alice",
      |  "age": 30,
      |  "active": true,
      |  "address": { "street": "123 Maple St", "city": "Austin" },
   "spouse":{"name": "Alice",
      |  "age": 30,
      |  "active": true,
      |  "address": { "street": "123 Maple St", "city": "Austin" }
    }
      |}
    """.stripMargin
  import JsonDecoder.*

  val personRowJson =
    """{"name":"Jack","age":1}
      |""".stripMargin

  val buf = json.getBytes(StandardCharsets.UTF_8)
  val res = summon[JsonDecoder[Person]].decode(buf, 0, buf.length)
  println(res)
  val buf1 = personRowJson.getBytes(StandardCharsets.UTF_8)
  val res2 = summon[JsonDecoder[PersonRow]].decode(buf1, 0, buf.length)
  println(res2)