package org.pwharned.json

import org.pwharned.database.hkd.{
  Default,
  GeneratedPrimaryKey,
  Nullable,
  PrimaryKey
}

import scala.deriving.*
import scala.compiletime.*
import scala.annotation.tailrec

// JsonDeserializer typeclass
trait JsonDeserializer[T]:
  def decode(buf: Array[Byte], pos: Int): (T, Int)

// low-level parsing helpers
object JsonDeserializer:
  inline def peek(buf: Array[Byte], pos: Int): Byte =
    if pos >= buf.length then throw new RuntimeException("Unexpected EOF")
    buf(pos)

  inline def expect(
      buf: Array[Byte],
      pos: Int,
      expected: Byte,
      what: String
  ): Int =
    if peek(buf, pos) != expected then
      throw new RuntimeException(s"Expected $what at $pos")
    pos + 1

  inline def advance(pos: Int, n: Int): Int = pos + n

  private inline def skipWhitespace(buf: Array[Byte], start: Int): Int =
    var pos = start
    val limit = buf.length
    while pos < limit && {
        val b = buf(pos)
        b == ' '.toByte || b == '\n'.toByte || b == '\r'.toByte || b == '\t'.toByte
      }
    do pos += 1
    pos

  def readStringSlice(buf: Array[Byte], start: Int): (Int, Int, Int) =
    var pos = start
    if pos >= buf.length || buf(pos) != '"'.toByte then
      throw new RuntimeException(s"Expected '\"' at $pos")
    pos += 1 // move past opening quote
    val s = pos
    val limit = buf.length
    var escaped = false
    while pos < limit do
      val b = buf(pos)
      if !escaped then
        if b == '\\'.toByte then
          escaped = true
          pos += 1
        else if b == '"'.toByte then
          val e = pos
          pos += 1 // move past closing quote
          return (s, e, pos)
        else pos += 1
      else
        // skip escaped char (treat as a single byte in the slice)
        escaped = false
        pos += 1
    throw new RuntimeException("Unterminated string")

  /** Compare a buffer slice buf[s:e] to a literal byte array `lit`. Returns
    * true if lengths match and every byte equals. This is minimal and
    * branch-friendly: check length, then loop.
    */
  def sliceEqualsBytes(
      buf: Array[Byte],
      s: Int,
      e: Int,
      lit: Array[Byte]
  ): Boolean =
    val len = e - s
    if len != lit.length then false
    else
      var i = 0
      // local val to help JIT eliminate bounds checks
      val base = s
      while i < len do
        if buf(base + i) != lit(i) then return false
        i += 1
      true

  inline def readOpenBrace(buf: Array[Byte], pos: Int): Int =
    expect(buf, pos, '{'.toByte, "'{'")
  inline def readCloseBrace(buf: Array[Byte], pos: Int): Int =
    expect(buf, pos, '}'.toByte, "'}'")
  inline def readColon(buf: Array[Byte], pos: Int): Int =
    expect(buf, pos, ':'.toByte, "':'")
  inline def readComma(buf: Array[Byte], pos: Int): Int =
    expect(buf, pos, ','.toByte, "','")
  inline def readQuote(buf: Array[Byte], pos: Int): Int =
    expect(buf, pos, '"'.toByte, "'\"'")

  // decode a JSON string (handles common escapes, no \uXXXX for brevity)
  inline def readString(buf: Array[Byte], start: Int): (String, Int) =
    var pos = readQuote(buf, start)
    val sb = new java.lang.StringBuilder
    val limit = buf.length
    var escaped = false
    var closed = false
    while pos < limit && !closed do
      val b = buf(pos)
      if !escaped then
        if b == '\\'.toByte then
          escaped = true
          pos += 1
        else if b == '"'.toByte then
          pos += 1
          closed = true
        else
          sb.append(b.toChar)
          pos += 1
      else
        b match
          case 34  => sb.append('"') // '"'
          case 92  => sb.append('\\') // '\'
          case 47  => sb.append('/') // '/'
          case 98  => sb.append('\b') // 'b'
          case 102 => sb.append('\f') // 'f'
          case 110 => sb.append('\n') // 'n'
          case 114 => sb.append('\r') // 'r'
          case 116 => sb.append('\t') // 't'
          case _   => throw new RuntimeException(s"Unsupported escape at $pos")
        escaped = false
        pos += 1
    if !closed then throw new RuntimeException("Unterminated string")
    (sb.toString, pos)

  // read a literal token (number, true, false, null) as string
  inline def readLiteral(buf: Array[Byte], start: Int): (String, Int) =
    var pos = start
    val sb = new java.lang.StringBuilder
    val limit = buf.length
    while pos < limit && {
        val c = buf(pos).toChar
        c != ',' && c != '}' && c != ']' && !c.isWhitespace
      }
    do
      sb.append(buf(pos).toChar)
      pos += 1
    (sb.toString, pos)

  // utility to skip a value generically (used for unknown fields)
  def skipValue(buf: Array[Byte], pos0: Int): Int =
    val pos = skipWhitespace(buf, pos0)
    if peek(buf, pos) == '"'.toByte then
      val (_, p) = readString(buf, pos)
      p
    else if peek(buf, pos) == '{'.toByte then
      var depth = 0
      var p = pos
      while p < buf.length do
        val b = buf(p)
        if b == '{'.toByte then depth += 1
        else if b == '}'.toByte then
          depth -= 1
          if depth == 0 then
            p += 1
            return p
        p += 1
      throw new RuntimeException("Unterminated object while skipping")
    else
      val (_, p) = readLiteral(buf, pos)
      p

  inline def apply[T](using d: JsonDeserializer[T]): JsonDeserializer[T] = d

  given [A](using d: JsonDeserializer[A]): JsonDeserializer[Option[A]] with
    def decode(buf: Array[Byte], pos: Int): (Option[A], Int) =
      val p = JsonDeserializer.skipWhitespace(buf, pos)
      if p < buf.length && JsonDeserializer.peek(buf, p) == 'n'.toByte then
        val (lit, next) = JsonDeserializer.readLiteral(buf, p)
        if lit == "null" then (None, next)
        else throw new RuntimeException(s"Invalid null literal: $lit")
      else
        val (v, next) = d.decode(buf, p)
        (Some(v), next)

  // Deserializer for List
  given [A](using d: JsonDeserializer[A]): JsonDeserializer[List[A]] with
    def decode(buf: Array[Byte], pos: Int): (List[A], Int) =
      var p = JsonDeserializer.skipWhitespace(buf, pos)
      p = JsonDeserializer.expect(buf, p, '['.toByte, "'['")
      p = JsonDeserializer.skipWhitespace(buf, p)
      val result = scala.collection.mutable.ListBuffer.empty[A]
      var done = false
      while !done do
        p = JsonDeserializer.skipWhitespace(buf, p)
        if p < buf.length && buf(p) == ']'.toByte then
          p = JsonDeserializer.expect(buf, p, ']'.toByte, "']'")
          done = true
        else
          val (value, next) = d.decode(buf, p)
          result += value
          p = JsonDeserializer.skipWhitespace(buf, next)
          if p < buf.length && buf(p) == ','.toByte then
            p = JsonDeserializer.expect(buf, p, ','.toByte, "','")
            p = JsonDeserializer.skipWhitespace(buf, p)
          else if p < buf.length && buf(p) == ']'.toByte then
            p = JsonDeserializer.expect(buf, p, ']'.toByte, "']'")
            done = true
          else throw new RuntimeException(s"Unexpected token at $p")
      (result.toList, p)
  // Deserializer for HKD Persisted wrapper

  given [A](using d: JsonDeserializer[A]): JsonDeserializer[Vector[A]] with
    def decode(buf: Array[Byte], pos: Int): (Vector[A], Int) =
      var p = JsonDeserializer.skipWhitespace(buf, pos)
      p = JsonDeserializer.expect(buf, p, '['.toByte, "'['")
      p = JsonDeserializer.skipWhitespace(buf, p)
      val result = scala.collection.mutable.ListBuffer.empty[A]
      var done = false
      while !done do
        p = JsonDeserializer.skipWhitespace(buf, p)
        if p < buf.length && buf(p) == ']'.toByte then
          p = JsonDeserializer.expect(buf, p, ']'.toByte, "']'")
          done = true
        else
          val (value, next) = d.decode(buf, p)
          result += value
          p = JsonDeserializer.skipWhitespace(buf, next)
          if p < buf.length && buf(p) == ','.toByte then
            p = JsonDeserializer.expect(buf, p, ','.toByte, "','")
            p = JsonDeserializer.skipWhitespace(buf, p)
          else if p < buf.length && buf(p) == ']'.toByte then
            p = JsonDeserializer.expect(buf, p, ']'.toByte, "']'")
            done = true
          else throw new RuntimeException(s"Unexpected token at $p")
      (result.toVector, p)
  given primaryKey[T](using
      underlying: JsonDeserializer[T]
  ): JsonDeserializer[PrimaryKey[T]] = (buf: Array[Byte], pos: Int) => {
    val decoded = underlying.decode(buf, pos)
    (PrimaryKey(decoded._1), decoded._2)
  }
  given default[T](using
      underlying: JsonDeserializer[T]
  ): JsonDeserializer[Default[T]] = (buf: Array[Byte], pos: Int) => {
    val decoded = underlying.decode(buf, pos)
    (Default(decoded._1), decoded._2)
  }
  given nullable[T](using
      underlying: JsonDeserializer[T]
  ): JsonDeserializer[Nullable[T]] = (buf: Array[Byte], pos: Int) => {
    val decoded = underlying.decode(buf, pos)
    (Nullable(decoded._1), decoded._2)
  }
  given generatedPrimaryKey[T](using
      underlying: JsonDeserializer[T]
  ): JsonDeserializer[GeneratedPrimaryKey[T]] = (buf: Array[Byte], pos: Int) =>
    {
      val decoded = underlying.decode(buf, pos)
      (GeneratedPrimaryKey(decoded._1), decoded._2)
    }
  // primitive decoders
  given JsonDeserializer[java.sql.Date] with
    def decode(buf: Array[Byte], pos: Int): (java.sql.Date, Int) =
      val decoded = JsonDeserializer.readString(buf, pos)
      val instant = java.time.Instant.parse(decoded._1)
      val date = new java.sql.Date(instant.getEpochSecond())
      (date, decoded._2)
  given JsonDeserializer[String] with
    def decode(buf: Array[Byte], pos: Int): (String, Int) =
      JsonDeserializer.readString(buf, pos)
  given JsonDeserializer[java.util.UUID] with
    def decode(buf: Array[Byte], pos: Int): (java.util.UUID, Int) = {
      val decoded = JsonDeserializer.readString(buf, pos)
      (java.util.UUID.fromString(decoded._1), decoded._2)
    }
  given JsonDeserializer[java.time.Instant] with
    def decode(buf: Array[Byte], pos: Int): (java.time.Instant, Int) = {
      val decoded = JsonDeserializer.readString(buf, pos)
      (java.time.Instant.parse(decoded._1), decoded._2)
    }
  given JsonDeserializer[Int] with
    def decode(buf: Array[Byte], pos: Int): (Int, Int) =
      val (lit, p) = JsonDeserializer.readLiteral(buf, pos)
      (lit.toInt, p)
  given JsonDeserializer[Float] with
    def decode(buf: Array[Byte], pos: Int): (Float, Int) =
      val (lit, p) = JsonDeserializer.readLiteral(buf, pos)
      (lit.toFloat, p)
  given JsonDeserializer[Long] with
    def decode(buf: Array[Byte], pos: Int): (Long, Int) =
      val (lit, p) = JsonDeserializer.readLiteral(buf, pos)
      (lit.toLong, p)
  given JsonDeserializer[scala.math.BigDecimal] with
    def decode(buf: Array[Byte], pos: Int): (scala.math.BigDecimal, Int) =
      val (lit, p) = JsonDeserializer.readLiteral(buf, pos)
      (scala.math.BigDecimal(lit), p)

  given JsonDeserializer[Double] with
    def decode(buf: Array[Byte], pos: Int): (Double, Int) =
      val (lit, p) = JsonDeserializer.readLiteral(buf, pos)
      (lit.toDouble, p)

  given JsonDeserializer[Boolean] with
    def decode(buf: Array[Byte], pos: Int): (Boolean, Int) =
      val (lit, p) = JsonDeserializer.readLiteral(buf, pos)
      val v = lit match
        case "true"  => true
        case "false" => false
        case other   => throw new RuntimeException(s"Invalid boolean: $other")
      (v, p)

  // Product derivation (case classes)
  inline given derived[T <: Product](using
      m: Mirror.ProductOf[T]
  ): JsonDeserializer[T] = productDecoder[T](m)
  inline def tupleSize[T <: Tuple]: Int =
    inline erasedValue[T] match
      case _: EmptyTuple => 0
      case _: (h *: t)   => 1 + tupleSize[t]

  inline def decodeFieldByIndex[Elems <: Tuple](
      idx: Int,
      buf: Array[Byte],
      pos: Int
  ): (Any, Int) =
    inline erasedValue[Elems] match
      case _: EmptyTuple =>
        throw new RuntimeException("No fields to decode")
      case _: (h *: EmptyTuple) =>
        if idx == 0 then
          summonInline[JsonDeserializer[h]]
            .decode(buf, pos)
            .asInstanceOf[(Any, Int)]
        else throw new RuntimeException("Index out of range")
      case _: (h *: t) =>
        if idx == 0 then
          summonInline[JsonDeserializer[h]]
            .decode(buf, pos)
            .asInstanceOf[(Any, Int)]
        else decodeFieldByIndex[t](idx - 1, buf, pos)

  inline def build[Elems <: Tuple]: Array[JsonDeserializer[Any]] =
    inline erasedValue[Elems] match
      case _: EmptyTuple => Array.empty
      case _: (hh *: tt) =>
        val hd =
          summonInline[JsonDeserializer[hh]].asInstanceOf[JsonDeserializer[Any]]
        val tail = build[tt]
        val arr = new Array[JsonDeserializer[Any]](1 + tail.length)
        arr(0) = hd
        System.arraycopy(tail, 0, arr, 1, tail.length)
        arr

  import scala.deriving.Mirror
  import scala.compiletime.{
    constValue,
    constValueTuple,
    erasedValue,
    summonInline
  }

  import scala.deriving.Mirror
  import scala.compiletime.{constValue, constValueTuple}

  inline def matchField[T](key: String): Int =
    inline summonInline[Mirror.ProductOf[T]] match
      case m =>
        val labels = constValueTuple[m.MirroredElemLabels]
        matchFieldRec[m.MirroredElemLabels](key, 0)

  inline def matchFieldRec[Labels <: Tuple](key: String, idx: Int): Int =
    inline erasedValue[Labels] match
      case _: EmptyTuple => -1
      case _: (h *: t) =>
        if key == constValue[h] then idx
        else matchFieldRec[t](key, idx + 1)

  inline def defaultTuple[Elems <: Tuple]: Tuple =
    inline erasedValue[Elems] match
      case _: EmptyTuple => EmptyTuple
      case _: (Option[?] *: tail) =>
        (None *: defaultTuple[tail])
      case _: (h *: tail) =>
        (null *: defaultTuple[tail])

  inline def productDecoder[T <: Product](
      m: Mirror.ProductOf[T]
  ): JsonDeserializer[T] =
    // build element decoders array at derivation time (one allocation per decoder instance)
    val length = tupleSize[m.MirroredElemTypes]
    val defaults = defaultTuple[m.MirroredElemTypes]
    val values = defaults.toArray.asInstanceOf[Array[Any]]
    val fieldLabels =
      constValueTuple[m.MirroredElemLabels] match
        case labels: Tuple =>
          labels.toArray.map(_.toString)

    (buf: Array[Byte], pos0: Int) =>
      var pos = JsonDeserializer.skipWhitespace(buf, pos0)
      pos = JsonDeserializer.readOpenBrace(buf, pos)
      pos = JsonDeserializer.skipWhitespace(buf, pos)

      var done = false

      while !done do
        pos = JsonDeserializer.skipWhitespace(buf, pos)
        if pos >= buf.length then throw new RuntimeException("Unexpected EOF")
        if buf(pos) == '}'.toByte then
          pos = JsonDeserializer.readCloseBrace(buf, pos)
          done = true
        else
          // read key
          val (key, posAfterKey) = JsonDeserializer.readString(buf, pos)
          pos = JsonDeserializer.skipWhitespace(buf, posAfterKey)
          pos = JsonDeserializer.readColon(buf, pos)
          pos = JsonDeserializer.skipWhitespace(buf, pos)

          // match field index (assumes macro-generated matchField exists)
          val idx = matchField[T](key)
          if idx >= 0 && idx < length then
            val (v, newPos) =
              decodeFieldByIndex[m.MirroredElemTypes](idx, buf, pos)
            values(idx) = v
            pos = JsonDeserializer.skipWhitespace(buf, newPos)
          else
            pos = JsonDeserializer.skipWhitespace(
              buf,
              JsonDeserializer.skipValue(buf, pos)
            )

          // consume optional comma
          pos = JsonDeserializer.skipWhitespace(buf, pos)
          if pos < buf.length && buf(pos) == ','.toByte then
            pos = JsonDeserializer.readComma(buf, pos)
            pos = JsonDeserializer.skipWhitespace(buf, pos)
          else ()
      var i = 0
      while i < length do
        if values(i) == null then
          throw new RuntimeException(
            s"Missing required field: ${fieldLabels(i)}"
          )
        i += 1
      val tuple = Tuple.fromArray(values)
      val product = m.fromProduct(tuple)
      (product.asInstanceOf[T], pos)

@main
def test(): Unit =
  case class Person(name: Option[String], age: Float, address: Address)
  case class Address(street: String)
  val deserialzier = JsonDeserializer.derived[Person]
  val string = """ { "age":1.23e4, "address": {"street":"Laurel Lane"} } """
  var i = 0
  while i < 10 do
    val p = deserialzier.decode(string.getBytes, 0)
    println(p)
