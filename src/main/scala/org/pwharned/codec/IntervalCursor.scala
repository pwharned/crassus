
package org.pwharned.codec
import java.nio.charset.StandardCharsets
import scala.collection.mutable.ArrayBuffer

object IntervalCursor:
  private inline def quote = '"'.toByte
  private inline def openBrace = '{'.toByte
  private inline def closeBrace = '}'.toByte
  

class IntervalCursor(buf: Array[Byte], sliceStart: Int, sliceEnd: Int) {
  private val UTF8 = StandardCharsets.UTF_8
  private lazy val intervals = ArrayBuffer((sliceStart, sliceEnd))
  private lazy val resultMap = scala.collection.mutable.Map.empty[String, Any]
  def extractField: Option[(Int, Int)] =
    val keyBytes = ("\"" + field + "\"").getBytes(UTF8)
    val keyLen = keyBytes.length

    // 1) find first interval containing the key
    var hitPos = -1
    var intervalI = -1
    var i = 0
    while i < intervals.length && hitPos < 0 do
      val (a, b) = intervals(i)
      val f = findSubArray(buf, keyBytes, a, b)
      if f >= 0 then
        hitPos = f
        intervalI = i
      else
        i += 1

    if hitPos < 0 then return None

    // 2) split interval: keep [a,hitPos) and (hitPos+keyLen,b)
    val (a, b) = intervals(intervalI)
    val splitEnd = hitPos + keyLen
    intervals.remove(intervalI)
    if a < hitPos then intervals.insert(intervalI, (a, hitPos))
    if splitEnd < b then
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
        var p = pos + 1
        while p < sliceEnd && depth > 0 do
          buf(p) match
            case quote if !inStr => inStr = true; p += 1
            case quote if inStr => inStr = false; p += 1
            case openBrace if !inStr => depth += 1; p += 1
            case closeBrace if !inStr => depth -= 1; p += 1
            case _ => p += 1
        (pos, p)

      else
        // number, boolean, or null
        var p = pos
        while p < sliceEnd && buf(p) != ','.toByte && buf(p) != '}'.toByte do
          p += 1
        (pos, p)

    Some((vs, ve))
    
    
  def extractField(field: String): Option[(Int, Int)] =
    val keyBytes = ("\"" + field + "\"").getBytes(UTF8)
    val keyLen = keyBytes.length

    // 1) find first interval containing the key
    var hitPos = -1
    var intervalI = -1
    var i = 0
    while i < intervals.length && hitPos < 0 do
      val (a, b) = intervals(i)
      val f = findSubArray(buf, keyBytes, a, b)
      if f >= 0 then
        hitPos = f
        intervalI = i
      else
        i += 1

    if hitPos < 0 then return None

    // 2) split interval: keep [a,hitPos) and (hitPos+keyLen,b)
    val (a, b) = intervals(intervalI)
    val splitEnd = hitPos + keyLen
    intervals.remove(intervalI)
    if a < hitPos then intervals.insert(intervalI, (a, hitPos))
    if splitEnd < b then
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
        var p = pos + 1
        while p < sliceEnd && depth > 0 do
          buf(p) match
            case quote if !inStr => inStr = true; p += 1
            case quote if inStr => inStr = false; p += 1
            case openBrace if !inStr => depth += 1; p += 1
            case closeBrace if !inStr => depth -= 1; p += 1
            case _ => p += 1
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
                            to: Int
                          ): Int =
    val max = to - needle.length
    var i = from
    while i <= max do
      var j = 0
      while j < needle.length && hay(i + j) == needle(j) do j += 1
      if j == needle.length then return i
      i += 1
    -1

  private def isWhitespace(b: Byte): Boolean =
    b == ' '.toByte || b == '\n'.toByte || b == '\r'.toByte || b == '\t'.toByte
}
