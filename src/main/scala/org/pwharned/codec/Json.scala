package org.pwharned.codec

import org.pwharned.codec

import java.nio.charset.StandardCharsets
import scala.deriving.Mirror
import scala.compiletime.{constValueTuple, erasedValue, error, summonInline}
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

object IntervalCursor:
  private inline def quote = '"'.toByte
  private inline def openBrace = '{'.toByte
  private inline def closeBrace = '}'.toByte

// ─── 1. ByteDecoder Typeclass ─────────────────────────────────────────────────

trait ByteDecoder[T]:
  def decode(buf: Array[Byte], start: Int, end: Int): Either[String, T]

object ByteDecoder:
  import java.nio.charset.StandardCharsets
  import scala.deriving.*
  import scala.compiletime.*

  // ─── Primitive Instances ─────────────────────────────────────────────────────

  given ByteDecoder[Int] with
    def decode(buf: Array[Byte], start: Int, end: Int): Either[String, Int] =
      if start >= end then Left("Empty int")
      else
        var i = start
        var sign = 1
        if buf(i) == '-'.toByte then { sign = -1; i += 1 }
        var acc = 0
        while i < end do
          val b = buf(i)
          if b >= '0'.toByte && b <= '9'.toByte then
            acc = acc * 10 + (b - '0'.toByte)
            i += 1
          else return Left(s"Invalid digit: ${b.toChar}")
        Right(sign * acc)

  given ByteDecoder[Double] with
    def decode(buf: Array[Byte], start: Int, end: Int): Either[String, Double] =
      val s = new String(buf, start, end - start, StandardCharsets.UTF_8)
      s.toDoubleOption.toRight(s"Invalid double: $s")

  given ByteDecoder[String] with
    def decode(buf: Array[Byte], start: Int, end: Int): Either[String, String] =
      if end - start >= 2 && buf(start) == '"'.toByte && buf(end - 1) == '"'.toByte then
        Right(new String(buf, start + 1, end - start - 2, StandardCharsets.UTF_8))
      else
        Left(s"Invalid string at [$start,$end)")

  given ByteDecoder[Boolean] with
    def decode(buf: Array[Byte], start: Int, end: Int): Either[String, Boolean] =
      val len = end - start
      if len == 4 &&
        buf(start) == 't' &&
        buf(start + 1) == 'r' &&
        buf(start + 2) == 'u' &&
        buf(start + 3) == 'e' then Right(true)
      else if len == 5 &&
        buf(start) == 'f' &&
        buf(start + 1) == 'a' &&
        buf(start + 2) == 'l' &&
        buf(start + 3) == 's' &&
        buf(start + 4) == 'e' then Right(false)
      else
        val invalid = new String(buf, start, len, StandardCharsets.UTF_8)
        Left(s"Invalid boolean: $invalid")

  // ─── Recursive-safe Option Decoder ───────────────────────────────────────────

  given optionDecoder[T](using dec: => ByteDecoder[T]): ByteDecoder[Option[T]] with
    def decode(buf: Array[Byte], start: Int, end: Int): Either[String, Option[T]] =
      val len = end - start
      if len == 4 &&
        buf(start) == 'n' &&
        buf(start + 1) == 'u' &&
        buf(start + 2) == 'l' &&
        buf(start + 3) == 'l' then Right(None)
      else dec.decode(buf, start, end).map(Some(_))

  // ─── Derivation ──────────────────────────────────────────────────────────────
  inline def isOptional[T]: Boolean =
    inline erasedValue[T] match
      case _: Option[?] => true
      case _ => false

  inline given derived[T](using m: Mirror.ProductOf[T]): ByteDecoder[T] =
    lazy val self: ByteDecoder[T] =
      val labels = getLabels[m.MirroredElemLabels]
      val decodersWithFlags = summonInstancesWithTypes[T, m.MirroredElemTypes](self)

      (buf: Array[Byte], start: Int, end: Int) =>
        val cursor = IntervalCursor(buf, start, end)
        val results = labels.zip(decodersWithFlags).map { case (name, (dec, isOpt)) =>
          cursor.extractField(name) match
            case Some((s, e)) =>
              dec.asInstanceOf[ByteDecoder[Any]].decode(buf, s, e)
            case None =>
              if isOpt then Right(None)
              else Left(s"Missing field: $name")
        }

        results.collectFirst { case Left(err) => Left(err) }
          .getOrElse {
            val vs = results.collect { case Right(v) => v }
            Right(m.fromProduct(Tuple.fromArray(vs.toArray)))
          }

    self


  // ─── Utilities ───────────────────────────────────────────────────────────────

  inline def summonInstances[T, Elems <: Tuple](self: => ByteDecoder[T]): List[ByteDecoder[?]] =
    inline erasedValue[Elems] match
      case _: (elem *: elems) => deriveOrSummon[T, elem](self) :: summonInstances[T, elems](self)
      case _: EmptyTuple      => Nil

  inline def summonInstancesWithTypes[T, Elems <: Tuple](self: => ByteDecoder[T]): List[(ByteDecoder[?], Boolean)] =
    inline erasedValue[Elems] match
      case _: (elem *: elems) =>
        val dec = deriveOrSummon[T, elem](self)
        val opt = isOptional[elem]
        (dec, opt) :: summonInstancesWithTypes[T, elems](self)
      case _: EmptyTuple => Nil

  inline def deriveOrSummon[T, Elem](self: => ByteDecoder[T]): ByteDecoder[Elem] =
    inline erasedValue[Elem] match
      case _: T => self.asInstanceOf[ByteDecoder[Elem]]
      case _    => summonInline[ByteDecoder[Elem]]

  inline def getLabels[T <: Tuple]: List[String] =
    constValueTuple[T].toList.asInstanceOf[List[String]]



// ─── 4. IntervalCursor (with Nested-Object Slicing) ──────────────────────────

class IntervalCursor(buf: Array[Byte], sliceStart: Int, sliceEnd: Int):
  private val UTF8       = StandardCharsets.UTF_8
  private val intervals  = ArrayBuffer((sliceStart, sliceEnd))

  def extractField(field: String): Option[(Int,Int)] =
    val keyBytes = ("\"" + field + "\"").getBytes(UTF8)
    val keyLen   = keyBytes.length

    // 1) find first interval containing the key
    var hitPos    = -1
    var intervalI = -1
    var i         = 0
    while i < intervals.length && hitPos < 0 do
      val (a,b) = intervals(i)
      val f     = findSubArray(buf, keyBytes, a, b)
      if f >= 0 then
        hitPos    = f
        intervalI = i
      else
        i += 1

    if hitPos < 0 then return None

    // 2) split interval: keep [a,hitPos) and (hitPos+keyLen,b)
    val (a, b)     = intervals(intervalI)
    val splitEnd   = hitPos + keyLen
    intervals.remove(intervalI)
    if a < hitPos      then intervals.insert(intervalI, (a, hitPos))
    if splitEnd < b    then
      val insertAt = if a < hitPos then intervalI + 1 else intervalI
      intervals.insert(insertAt, (splitEnd, b))

    // 3) parse JSON value at hitPos
    var pos = hitPos + keyLen
    while pos < sliceEnd && buf(pos) != ':'.toByte do pos += 1
    pos += 1
    while pos < sliceEnd && isWhitespace(buf(pos)) do pos += 1
    val valueStart = pos

    val (vs, ve) =
      if pos < sliceEnd && buf(pos) == '"'.toByte then
        // string literal (handle escapes simply)
        var p = pos + 1
        while p < sliceEnd && buf(p) != '"'.toByte do
          if buf(p) == '\\'.toByte then p += 1
          p += 1
        (pos, p + 1)

      else if pos < sliceEnd && buf(pos) == '{'.toByte then
        // nested object: balance braces, ignore quoted sections
        var depth = 1
        var inStr = false
        var p     = pos + 1
        while p < sliceEnd && depth > 0 do
          buf(p) match
            case quote if !inStr => inStr = true;   p += 1
            case quote if inStr  => inStr = false;  p += 1
            case openBrace if !inStr => depth += 1;     p += 1
            case closeBrace if !inStr => depth -= 1;     p += 1
            case _                    => p += 1
        (pos, p)

      else
        // number, boolean, or null
        var p = pos
        while p < sliceEnd && buf(p) != ','.toByte && buf(p) != '}'.toByte do
          p += 1
        (pos, p)

    Some((vs, ve))

  private def findSubArray(
                            hay: Array[Byte],
                            needle: Array[Byte],
                            from: Int,
                            to:   Int
                          ): Int =
    val max = to - needle.length
    var i   = from
    while i <= max do
      var j = 0
      while j < needle.length && hay(i+j) == needle(j) do j += 1
      if j == needle.length then return i
      i += 1
    -1

  private def isWhitespace(b: Byte): Boolean =
    b == ' '.toByte || b == '\n'.toByte || b == '\r'.toByte || b == '\t'.toByte

// ─── 5. Example: Nested Case Classes ──────────────────────────────────────────

@main def runNested(): Unit =

  case class Address(street: String, city: String)
  case class Person(
                     name: String,
                     age: Int,
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
  import ByteDecoder.*

  val personRowJson =
    """{"name":"Jack","age":1}
      |""".stripMargin

  val buf = json.getBytes(StandardCharsets.UTF_8)
  val res = summon[ByteDecoder[Person]].decode(buf, 0, buf.length)
  println(res)
  val buf1 = personRowJson.getBytes(StandardCharsets.UTF_8)
  val res2 = summon[ByteDecoder[PersonRow]].decode(buf1, 0, buf.length)
  println(res2)