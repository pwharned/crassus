package org.pwharned.codec



import org.ibm.pwhaned.codec.IntervalCursor
import org.pwharned.codec.dispatchBytesMacro

import scala.collection.mutable.ArrayBuffer
import scala.compiletime.{constValueTuple, erasedValue, summonInline}
import scala.deriving.Mirror
import scala.language.implicitConversions

// ─── 1. ByteDecoder Typeclass ─────────────────────────────────────────────────

trait JsonDecoder[T]:
  def decode(buf: Array[Byte], start: Int, end: Int): T

object JsonDecoder  {
  import java.nio.charset.StandardCharsets
  import scala.compiletime.*
  import scala.deriving.*

  // ─── Primitive Instances ─────────────────────────────────────────────────────

  given JsonDecoder[Int] with
    def decode(buf: Array[Byte], start: Int, end: Int):  Int=
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
    def decode(buf: Array[Byte], start: Int, end: Int): Long =
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
    def decode(buf: Array[Byte], start: Int, end: Int): Double =
      var i = start
      var result = 0.0
      var sign = 1.0
      var decimalMultiplier = 0.1
      var pastDecimal = false
      var pastExponent = false
      var exponent = 0
      var exponentSign = 1

      // Handle sign
      if i < end && buf(i) == '-'.toByte then {
        sign = -1.0
        i += 1
      }

      while i < end do
        val b = buf(i)
        if b == '.'.toByte then
          if pastDecimal || pastExponent then throw new Exception("Invalid decimal point position")
          pastDecimal = true
        else if (b == 'E'.toByte || b == 'e'.toByte) then
          if pastExponent then throw new Exception("Multiple exponent markers")
          pastExponent = true
          // Check for exponent sign
          if i + 1 < end then
            val nextB = buf(i + 1)
            if nextB == '-'.toByte then
              exponentSign = -1
              i += 1
            else if nextB == '+'.toByte then
              exponentSign = 1
              i += 1
        else if b >= '0'.toByte && b <= '9'.toByte then
          val digit = (b - '0'.toByte)
          if pastExponent then
            exponent = exponent * 10 + digit
          else
            val digitDouble = digit.toDouble
            if pastDecimal then
              result += digitDouble * decimalMultiplier
              decimalMultiplier *= 0.1
            else
              result = result * 10.0 + digitDouble
        else {
          val s = new String(buf, start, end - start)
          throw new Exception(s"Invalid float character at position ${i - start}: $s")
        }
        i += 1
      val finalExponent = exponent * exponentSign
      val finalResult = if finalExponent != 0 then
        result * math.pow(10.0, finalExponent)
      else
        result

      sign * finalResult

  given JsonDecoder[Float] with
    def decode(buf: Array[Byte], start: Int, end: Int): Float =
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
    def decode(buf: Array[Byte], start: Int, end: Int): String = {

      new String(buf, start + 1, end - start - 2, StandardCharsets.UTF_8)
    }

  given JsonDecoder[Boolean] with
    def decode(buf: Array[Byte], start: Int, end: Int): Boolean =
      val len = end - start
      if len == 4 then true
      else if len == 5 then false
      else
        throw new RuntimeException(s"Invalid boolean at ${start}: ${end}")

  // ─── Recursive-safe Option Decoder ───────────────────────────────────────────

  given listDecoder[T](using dec: JsonDecoder[T]): JsonDecoder[List[T]] with
    def decode(buf: Array[Byte], start: Int, end: Int): List[T] =
      val cursor = new IntervalCursor(buf)
      cursor.skipToArrayStart(start, end)

      val result = ArrayBuffer[T]()
      while cursor.hasMoreArrayElements do
        val (elemStart, elemEnd) = cursor.nextArrayElement()
        result += dec.decode(buf, elemStart, elemEnd)

      result.toList
  given optionDecoder[T](using dec: JsonDecoder[T]): JsonDecoder[Option[T]] with
    def decode(buf: Array[Byte], start: Int, end: Int): Option[T] =
      val len = end - start
      if len == 4 &&
        buf(start) == 'n' &&
        buf(start + 1) == 'u' &&
        buf(start + 2) == 'l' &&
        buf(start + 3) == 'l' then None
      else Some(dec.decode(buf, start, end))

  given productOptional[T<:Product](using dec: JsonDecoder[T]): JsonDecoder[Option[T]] with
    def decode(buf: Array[Byte], start: Int, end: Int): Option[T] =
      Option(dec.decode(buf, start, end))


  inline given derived[T<:Product](using m: Mirror.ProductOf[T]):  JsonDecoder[T] =
    // Create field name to index mapping at compile time
    lazy val matchFunction = dispatchBytesMacro[T]
    lazy val self: JsonDecoder[T] =
      lazy val decoders: Vector[JsonDecoder[Any]] =
        summonInstancesWithTypes[T, m.MirroredElemTypes](self)
          .map(_._1.asInstanceOf[JsonDecoder[Any]])
          .toVector
      (buf: Array[Byte], start: Int, end: Int) =>
        val cursor = new IntervalCursor(buf)
        cursor.skipToObjectStart(start, end)
        val resultArr = Array.ofDim[Any](decoders.length)
        var fieldsFound = 0
        while cursor.hasMoreObjectFields && fieldsFound < decoders.length do
          val ((kStart, kEnd), (vStart, vEnd)) = cursor.nextField()
          // FIX: Extract only the field name portion, removing quotes
          val fieldName = new String(buf, kStart, kEnd - kStart, StandardCharsets.UTF_8)
          val idx = matchFunction(fieldName)
          if idx >= 0 then
            resultArr(idx) = decoders(idx).decode(buf, vStart, vEnd)
            fieldsFound += 1
        m.fromProduct(Tuple.fromArray(resultArr))
    self

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

  // ─── Derivation ──────────────────────────────────────────────────────────────
  inline def isOptional[T]: Boolean =
    inline erasedValue[T] match
      case _: Option[?] => true
      case _ => false


}

