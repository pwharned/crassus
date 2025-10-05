import io.circe.jawn.decode
import org.pwharned.codec.{JsonDecoder, JsonEncoder}

import java.nio.{ByteBuffer, ByteOrder}

case class Person(name: String, age: Int, isActive: Boolean)

object PersonParser {
  // 8-byte zero-byte trick constants
  private val ONES:     Long = 0x0101010101010101L
  private val HIGHBITS: Long = 0x8080808080808080L

  @inline private def repeat(b: Byte): Long = {
    val v = b & 0xFFL
    v       | (v <<  8) | (v << 16) | (v << 24) |
      (v << 32) | (v << 40) | (v << 48) | (v << 56)
  }

  /** Build the zero-byte mask for a target byte. */
  @inline private def zeroMask(chunk: Long, targMask: Long): Long = {
    val x = chunk ^ targMask
    (x - ONES) & ~x & HIGHBITS
  }

  /**
   * Find the next occurrence of `ch` in `buf` at or after `start`.
   * Uses 8-byte scans, then falls back to single-byte.
   */
  private def findNext(buf: ByteBuffer, start: Int, ch: Byte): Int = {
    val lim8     = buf.limit() - 8
    val targMask = repeat(ch)
    var i        = start

    // 8-byte leaps
    while (i <= lim8) {
      val chunk = buf.getLong(i)
      val m     = zeroMask(chunk, targMask)
      if (m != 0L) {
        // locate byte‐lane: bit 7 of the matching lane is set
        val bitPos = java.lang.Long.numberOfTrailingZeros(m)
        return i + (bitPos >>> 3)
      }
      i += 8
    }

    // tail‐scan last <8 bytes
    while (i < buf.limit()) {
      if (buf.get(i) == ch) return i
      i += 1
    }
    -1
  }

  /** Skip any bytes in `set` (e.g. whitespace or colon) starting at `pos`. */
  private def skipAny(buf: ByteBuffer, pos: Int, set: Set[Byte]): Int = {
    var i = pos
    while (i < buf.limit() && set(buf.get(i))) i += 1
    i
  }

  /** Parse an integer starting at `pos`; returns (value, nextPos). */
  private def parseInt(buf: ByteBuffer, pos: Int): (Int, Int) = {
    var i    = pos
    var neg  = false
    if (buf.get(i) == '-') { neg = true; i += 1 }
    var acc  = 0
    while (i < buf.limit()) {
      val b = buf.get(i)
      if (b < '0' || b > '9') return (if (neg) -acc else acc, i)
      acc = acc * 10 + (b - '0')
      i  += 1
    }
    (if (neg) -acc else acc, i)
  }

  /** Parse a boolean (`true` or `false`) at `pos`. */
  private def parseBool(buf: ByteBuffer, pos: Int): (Boolean, Int) = {
    val start = pos
    if (buf.get(start) == 't') (true, start + 4)   // "true"
    else                    (false, start + 5)    // "false"
  }

  /** Parse a JSON string starting at the opening quote `pos`. */
  private def parseString(buf: ByteBuffer, pos: Int): (String, Int) = {
    // find closing quote
    val endQuote = findNext(buf, pos + 1, '"')
    // extract bytes between pos+1 and endQuote (ignoring escapes)
    val len       = endQuote - (pos + 1)
    val slice     = new Array[Byte](len)
    buf.position(pos + 1)
    buf.get(slice, 0, len)
    (new String(slice, "UTF-8"), endQuote + 1)
  }

  /** Top-level parser for Person */
  def parsePerson(json: String): Person = {
    // wrap & set LE so getLong(i) works predictably
    val bytes = json.getBytes("UTF-8")
    val buf   = ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN)

    // 1) Find `"name"` key
    var pos = findNext(buf, 0, '"')
    val (nameKey, p1) = parseString(buf, pos)
    require(nameKey == "name", s"expected name key, got $nameKey")

    // skip whitespace + colon
    pos = skipAny(buf, p1, Set(' ', '\n', '\r', '\t'))
    require(buf.get(pos) == ':'); pos += 1
    pos = skipAny(buf, pos, Set(' ', '\n', '\r', '\t'))

    // 2) Parse name value
    val (nameValue, p2) = parseString(buf, pos)

    // find `"age"` key next
    pos = findNext(buf, p2, '"')
    val (ageKey, p3) = parseString(buf, pos)
    require(ageKey == "age", s"expected age key, got $ageKey")

    pos = skipAny(buf, p3, Set(' ', '\n', '\r', '\t')); require(buf.get(pos) == ':'); pos += 1
    pos = skipAny(buf, pos, Set(' ', '\n', '\r', '\t'))

    // 3) Parse age value
    val (ageValue, p4) = parseInt(buf, pos)

    // find `"isActive"` key
    pos = findNext(buf, p4, '"')
    val (activeKey, p5) = parseString(buf, pos)
    require(activeKey == "isActive", s"expected isActive key, got $activeKey")

    pos = skipAny(buf, p5, Set(' ', '\n', '\r', '\t')); require(buf.get(pos) == ':'); pos += 1
    pos = skipAny(buf, pos, Set(' ', '\n', '\r', '\t'))

    // 4) Parse boolean value
    val (boolValue, _) = parseBool(buf, pos)

    Person(nameValue, ageValue, boolValue)
  }

  // Quick smoke-test
  def main(args: Array[String]): Unit = {
    import io.circe._, io.circe.generic.semiauto._, io.circe.syntax._
    implicit val fooDecoder: Decoder[Person] = deriveDecoder[Person]


    inline def time[T](inline block: T): T = {
      val start = System.nanoTime()
      val result = block
      val duration = System.nanoTime() - start
      println(s"Execution time: ${duration / 1_000_000.0} ms")
      result
    }
    val json = """{"name":"AliceInWonder","age":30,"isActive":true}"""
    val jsonDecoder = summon[JsonDecoder[Person]]
    val jsonEncoder = summon[JsonEncoder[Person]]
    val bytes = json.getBytes

    time {
      0 to 1 foreach { x=>
        val s: Array[Byte] = jsonEncoder.encode(jsonDecoder.decode(bytes, 0, bytes.length))
        val string = new String(s, 0, s.length)
        println(string)

      }
    }



    // Person(Alice,30,true)
  }
}
