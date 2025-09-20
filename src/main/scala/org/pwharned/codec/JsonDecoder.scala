package org.pwharned.codec



import io.circe.{Decoder, Encoder, Json}
import io.circe.generic.semiauto.*
import io.circe.parser.*

import java.nio.charset.StandardCharsets
import scala.annotation.tailrec
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

        else throw {
          val string = String(buf.slice(start, end))
          Exception(s"Invalid integer : ${string} at ${start}: ${end}")
        }
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
        else throw {
          val string = String(buf.slice(start, end))
          Exception(s"Invalid integer : ${string} at ${start}: ${end}")
        }
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
        new String(buf, start + 1, end - start - 2, StandardCharsets.UTF_8)

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
    // 1) Precompute the sum‐of‐bytes for each label
    val labelSums: Vector[Int] =
      getLabels[m.MirroredElemLabels].iterator
        .map(_.getBytes(StandardCharsets.UTF_8).map(b => b & 0xFF).sum)
        .toVector


    // 2) Build a tiny "perfect hash" table: pick a size > 2*N to avoid collisions
    val tableSize = labelSums.length * 2 + 1
    val jump: Array[Int] = Array.fill(tableSize)(-1)
    for i <- labelSums.indices do
      val bucket = labelSums(i) % tableSize
      jump(bucket) = i

    lazy val self: JsonDecoder[T] =
      // decoders in the same order as `labels`
      val decoders: Vector[JsonDecoder[Any]] =
        summonInstancesWithTypes[T, m.MirroredElemTypes](self)
          .map(_._1.asInstanceOf[JsonDecoder[Any]])
          .toVector

      (buf: Array[Byte], start: Int, end: Int) =>
        val cursor = new IntervalCursor(buf)
        // position cursor at first '{'
        cursor.skipToObjectStart(start, end)

        // prepare result array
        val resultArr = Array.ofDim[Any](labelSums.length)

        // parse exactly `labels.length` fields inline
        var i = 0
        while i < labelSums.length do
          val ((kStart, kEnd), (vStart, vEnd)) = cursor.nextField()
          var s = 0
          var j = kStart
          while j < kEnd do
            s += buf(j) & 0xFF
            j += 1

          // perfect-hash dispatch
          val bucket = s % tableSize
          val idx = jump(bucket)
          // collision check
          if idx < 0 || labelSums(idx) != s then
            throw new IllegalArgumentException(s"Unknown JSON key sum: $s")

          // decode into the proper slot
          resultArr(idx) = decoders(idx).decode(buf, vStart, vEnd)
          i += 1

        // build your product
        m.fromProduct(Tuple.fromArray(resultArr))

    self

  // ─── Utilities ───────────────────────────────────────────────────────────────
  inline def fnv1aHash(inline s: String): Long = {
    // 64-bit FNV-1a constants
    val OffsetBasis = 1469598103934665603L
    val Prime = 1099511628211L

    // recursive, inline loop over string characters
    @tailrec
    def loop(i: Int, hash: Long): Long =
      if i < s.length then
        // mix in next byte
        loop(i + 1, (hash ^ s.charAt(i).toLong) * Prime)
      else
        hash

    loop(0, OffsetBasis)
  }
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





@main def runNested(): Unit =

  import com.github.plokhotnyuk.jsoniter_scala.core.*
  import com.github.plokhotnyuk.jsoniter_scala.macros.*


  object Person:
    given codec: JsonValueCodec[Person] = JsonCodecMaker.make


  case class Address(street: String, city: String)
  case class Person(
                     name: String,
                     age: Int,
                     active: Boolean,
                     score: Float,
                   )

  type PersonRow = (name: String,age:Int, active: Boolean, score: Float)

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
    """{"name":"Jack","age":1, "active":true, "score": 0.1}
      |""".stripMargin

  val buf = personRowJson.getBytes(StandardCharsets.UTF_8)
  val res = summon[JsonDecoder[Person]].decode(buf, 0, buf.length)

  object BenchmarkManual {
    // sample JSON
    val jsonStr = """{"name":"Alice","age":30,"active":true, "score": 0.1}"""
    val customDecoder = summon[JsonDecoder[PersonRow]]

    def time[R](label: String)(block: => R): R = {
      val start = System.nanoTime()
      val result = block
      val elapsed = System.nanoTime() - start
      println(f"$label: ${elapsed / 1e6}%.2f ms")
      result
    }
    implicit val decoder: Decoder[Person] = deriveDecoder[Person]

    def main: Unit = {
      val buf = jsonStr.getBytes(StandardCharsets.UTF_8)
      val runs = 10000000

      // warmup
      (1 to 5).foreach(_ =>
        customDecoder.decode(buf, 0, buf.length)
        decode[Person](jsonStr).getOrElse(sys.error("fail"))
      )
      // measure circe
      time("Circe generic") {
        var i = 0
        while i < runs do
          decode[Person](jsonStr).getOrElse(sys.error("fail"))
          i += 1
      }
      // measure custom
      time("Custom parser") {
        var i = 0
        while i < runs do
          customDecoder.decode(buf, 0, buf.length)
          i += 1
      }
      time("Jsoniter parser") {
        var i = 0
        while i < runs do
          val deserialized: Person = readFromString[Person](jsonStr)

          i += 1
      }

    }
  }
  BenchmarkManual.main