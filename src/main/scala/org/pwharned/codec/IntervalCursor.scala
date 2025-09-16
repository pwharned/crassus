package org.pwharned.codec
import java.nio.charset.StandardCharsets

class IntervalCursor(buf: Array[Byte]) {
  import IntervalCursor._

  private val UTF8 = StandardCharsets.UTF_8
  var pos   = 0
  val length = buf.length
  lazy val resultMap = scala.collection.mutable.ArrayBuffer.empty[(String, (Int, Int))]

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

  inline def skipToObjectStart(start: Int, end: Int): Unit = {
    pos = start
    while pos < end && buf(pos) != openBrace do pos += 1
    if pos < end then pos += 1
  }

  /** This replaces extractFieldNoReturn.  Returns:
   *   ((keyStart, keyEnd), (valueStart, valueEnd))
   * where keyEnd/valueEnd are exclusive indices.
   */
  inline def nextField(): ((Int, Int), (Int, Int)) = {
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
  inline private def isWs(b: Byte) =
    b == 32 || b == 9 || b == 10 || b == 13

  private inline def parseString(): (Int, Int) = {
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

  private inline def parseNested(open: Byte, close: Byte): (Int, Int) = {
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

  private inline def parsePrimitive(): (Int, Int) = {
    val vs = pos
    while pos < length &&
      buf(pos) != comma && buf(pos) != closeBrace &&
      !isWs(buf(pos)) do
      pos += 1
    (vs, pos)
  }




  // Helpers
  private inline def skipWhile(p: Byte => Boolean): Unit =
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
