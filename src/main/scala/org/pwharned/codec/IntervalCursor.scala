package org.ibm.pwhaned.codec

import java.nio.charset.StandardCharsets
import scala.collection.mutable.ArrayBuffer

final class IntervalCursor(buf: Array[Byte]) {
  import IntervalCursor._

  private val UTF8 = StandardCharsets.UTF_8
  var pos   = 0
  val length = buf.length
  lazy val resultMap = scala.collection.mutable.ArrayBuffer.empty[(String, (Int, Int))]

  inline def skipWs() = {
    while (isWs(buf(pos))) {
      pos +=1
    }
  }




  def parseInline(buf: Array[Byte]): ArrayBuffer[(String, (Int, Int))] = {
    // Constants



    val length = buf.length
    var pos = 0
    var inObject = false
    val resultMap = ArrayBuffer.empty[(String, (Int, Int))]

    // One loop to do it all
    while (pos < length) {
      val b = buf(pos)

      // 1) Haven't seen '{' yet? Skip until we do.
      if (!inObject) {
        if (b == openBrace) inObject = true
        pos += 1

      } else {
        // 2) We're inside the object.  If we hit '}', we're done.
        if (b == closeBrace) return resultMap

        // 3) Skip any whitespace or commas before the next key
        while (pos < length && (isWs(buf(pos)) || buf(pos) == comma)) pos += 1

        // 4) Parse the key string: assume buf(pos) == '"'
        require(pos < length && buf(pos) == quote, s"Expected '\"' at $pos")
        val kStart = pos + 1 // first char of key
        pos += 1 // skip the opening quote
        while (pos < length && buf(pos) != quote) {
          if (buf(pos) == backslash) pos += 2 // skip escaped char
          else pos += 1
        }
        val kEnd = pos // end of key (exclusive)
        pos += 1 // skip the closing quote
        val key = new String(buf, kStart, kEnd - kStart)

        // 5) Skip whitespace and the colon separating key and value
        while (pos < length && (isWs(buf(pos)) || buf(pos) == colon)) pos += 1

        // 6) Parse the value, record its start index
        val vStart = pos
        buf(pos) match {
          // --- String value ---
          case `quote` =>
            pos += 1
            while (pos < length && buf(pos) != quote) {
              if (buf(pos) == backslash) pos += 2
              else pos += 1
            }
            pos += 1

          // --- Nested object ---
          case `openBrace` =>
            var depth = 1
            var inStr = false
            pos += 1
            while (pos < length && depth > 0) {
              buf(pos) match {
                case `quote` if !inStr => inStr = true; pos += 1
                case `quote` if inStr => inStr = false; pos += 1
                case `backslash` if inStr => pos += 2
                case b if b == openBrace && !inStr => depth += 1; pos += 1
                case b if b == closeBrace && !inStr => depth -= 1; pos += 1
                case _ => pos += 1
              }
            }

          // --- Nested array ---
          case `openBracket` =>
            var depth = 1
            var inStr = false
            pos += 1
            while (pos < length && depth > 0) {
              buf(pos) match {
                case `quote` if !inStr => inStr = true; pos += 1
                case `quote` if inStr => inStr = false; pos += 1
                case `backslash` if inStr => pos += 2
                case b if b == openBracket && !inStr => depth += 1; pos += 1
                case b if b == closeBracket && !inStr => depth -= 1; pos += 1
                case _ => pos += 1
              }
            }

          // --- Primitive: number, true, false, null ---
          case _ =>
            while (pos < length &&
              buf(pos) != comma &&
              buf(pos) != closeBrace &&
              buf(pos) != closeBracket &&
              !isWs(buf(pos))) {
              pos += 1
            }
        }

        // 7) Record the end index, then stash the interval
        val vEnd = pos
        resultMap += ((key, (vStart, vEnd)))

        // 8) Skip any trailing whitespace or commas before looping again
        while (pos < length && (isWs(buf(pos)) || buf(pos) == comma)) pos += 1
      }
    }

    resultMap
  }

  inline def parse(): Unit = {
    // 1) Skip to '{'
    while (pos < length && buf(pos) != openBrace) pos += 1
    if (pos < length) pos += 1

    // 2) Main loop: read fields until '}'
    while (pos < length && buf(pos) != closeBrace) {
      extractField()

      // Eat trailing whitespace or commas
      skipWhile(b => isWs(b) || b == comma)
    }
  }

  private def extractField(): Unit = {
    skipWhile(isWs)
    // 1) --- Key (always a JSON string) ---
    require(pos < length && buf(pos) == quote, "Expected '\"' at key start")
    val kStart = pos + 1
    pos += 1
    // Find closing quote, honouring escapes
    while (pos < length && buf(pos) != quote) {
      if (buf(pos) == backslash) pos += 2
      else pos += 1
    }
    val kEnd = pos
    if (pos < length) pos += 1        // skip closing quote
    val key  = new String(buf, kStart, kEnd - kStart, UTF8)

    // 2) Skip whitespace + colon
    skipWhile(b => isWs(b) || b == colon)

    // 3) --- Value (string, object, array or primitive) ---
    if (pos >= length) return
    val interval: (Int, Int) = buf(pos) match {
      case `quote`      => parseString()
      case `openBrace`  => parseNested(openBrace, closeBrace)
      case `openBracket`=> parseNested(openBracket, closeBracket)
      case _            => parsePrimitive()
    }

    resultMap += ((key, interval))
  }

  def skipToObjectStart(start: Int, end: Int): Unit = {
    pos = start
    while pos < end && buf(pos) != openBrace do pos += 1
    if pos < end then pos += 1
  }

  /** This replaces extractFieldNoReturn.  Returns:
   *   ((keyStart, keyEnd), (valueStart, valueEnd))
   * where keyEnd/valueEnd are exclusive indices.
   */
  def nextField(): ((Int, Int), (Int, Int)) = {
    // 1) Skip any commas or whitespace before the key
    while pos < length && (isWs(buf(pos)) || buf(pos) == comma) do
      pos += 1

    // 2) Parse the key (must be a JSON string)
    require(pos < length && buf(pos) == quote, s"Expected '\"' at $pos")
    val kStart = pos + 1
    pos += 1
    while pos < length && buf(pos) != quote do
      if buf(pos) == backslash then pos += 2
      else pos += 1
    val kEnd = pos
    pos += 1 // skip closing quote

    // 3) Skip whitespace + the colon
    while pos < length && (isWs(buf(pos)) || buf(pos) == colon) do
      pos += 1

    // 4) Parse the value, returning its half-open interval
    val (vStart, vEnd) =
      buf(pos) match
        case `quote`       => parseString()
        case `openBrace`   => parseNested(openBrace, closeBrace)
        case `openBracket` => parseNested(openBracket, closeBracket)
        case _             => parsePrimitive()
    ((kStart, kEnd), (vStart, vEnd))
  }

  // --- helper methods copied from before ---
  def isWs(b: Byte) =
    b == 32 || b == 9 || b == 10 || b == 13

  def parseString(): (Int, Int) = {
    // pos is at the opening quote
    val vs = pos // include the open-quote
    pos += 1 // skip it

    // skip through the string, honouring escapes
    while pos < length && buf(pos) != quote do
      if buf(pos) == backslash then pos += 2
      else pos += 1

    pos += 1 // skip the closing quote
    val ve = pos // ve is one past the close-quote
    (vs, ve) // half-open [vs, ve) includes both quotes
  }

  def parseNested(open: Byte, close: Byte): (Int, Int) = {
    val vs = pos
    var depth = 1
    var inStr = false
    pos += 1
    while pos < length && depth > 0 do
      buf(pos) match
        case `quote` if !inStr    => inStr = true;  pos += 1
        case `quote` if inStr     => inStr = false; pos += 1
        case `backslash` if inStr => pos += 2
        case b if b == open  && !inStr => depth += 1; pos += 1
        case b if b == close && !inStr => depth -= 1; pos += 1
        case _                     => pos += 1
    (vs, pos)
  }


  def parsePrimitive(): (Int, Int) = {
    val vs = pos
    while pos < length &&
      buf(pos) != comma &&
      buf(pos) != closeBrace &&
      buf(pos) != closeBracket && // <- Add this for arrays
      !isWs(buf(pos)) do
      pos += 1
    (vs, pos)
  }

  def skipToArrayStart(start: Int, end: Int): Unit = {
    pos = start
    while pos < end && buf(pos) != openBracket do pos += 1
    if pos < end then pos += 1 // skip '['
  }

  /** Get the next array element as (start, end) indices */
  def nextArrayElement(): (Int, Int) = {
    // Skip any commas or whitespace before the element
    while pos < length && (isWs(buf(pos)) || buf(pos) == comma) do
      pos += 1
    // Parse the element value
    val (elemStart, elemEnd) = buf(pos) match
      case `quote` => parseString()
      case `openBrace` => parseNested(openBrace, closeBrace)
      case `openBracket` => parseNested(openBracket, closeBracket)
      case _ => parsePrimitive()

    (elemStart, elemEnd)
  }

  /** Check if there are more array elements (not at ']') */
  def hasMoreArrayElements: Boolean = {
    skipWhile(isWs)
    pos < length && buf(pos) != closeBracket
  }

  /** Check if there are more object fields (not at '}') */
  def hasMoreObjectFields: Boolean = {
    skipWhile(isWs)
    pos < length && buf(pos) != closeBrace
  }


  // Helpers
  private def skipWhile(p: Byte => Boolean): Unit =
    while (pos < length && p(buf(pos))) pos += 1
}

object IntervalCursor {

  private inline val quote       = 34
  private inline val backslash   = 92
  private inline val openBrace   = 123
  private inline val closeBrace  = 125
  private inline val openBracket = 91
  private inline val closeBracket= 93
  private inline val colon       = 58
  private inline val comma       = 44

  private inline def isWs(b: Byte): Boolean =
    b == 32 || b == 9 || b == 10 || b == 13

  private inline def isDelimiter(b: Byte): Boolean =
    b == comma || b == closeBrace || isWs(b)
}
