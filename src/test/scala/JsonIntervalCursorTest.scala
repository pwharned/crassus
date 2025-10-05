
package org.ibm.json

import org.ibm.pwhaned.codec.IntervalCursor

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.UTF_8

object IntervalCursorTest {
  def main(args: Array[String]): Unit = {
    // Sample JSON covering all value‐types
    val json =
      """{"a":1,"b":"two","c":[3,4],"d":{"e":5},"f":true,"g":null}"""
    val buf    = json.getBytes(UTF_8)


    // 1) Run our cursor
    val cursor = new IntervalCursor(buf)
    cursor.parse()

    // 2) Helper to slice out the substring for a given key
    def value(key: String): String = {
      cursor.resultMap
        .find(_._1 == key)
        .map { case (_, (start, end)) =>
          new String(buf, start, end - start, UTF_8)
        }
        .getOrElse(sys.error(s"Key not found: $key"))
    }

    // 3) Assertions
    assert(value("a") == "1")
    assert(value("b") == "\"two\"")    // includes quotes
    assert(value("c") == "[3,4]")
    assert(value("d") == "{\"e\":5}")
    assert(value("f") == "true")
    assert(value("g") == "null")

    // 4) Ensure we got exactly 6 fields
    assert(cursor.resultMap.size == 6)

    println("✓ IntervalCursor basic smoke‐test passed")
  }


  val ONES = 0x0101010101010101L
  val HIGHBITS = 0x8080808080808080L
  val CLOSE_ = '}'.toByte
  val MASK = (CLOSE_ & 0xFF).toLong * ONES
  
  0 to 10000 foreach {
    x => {
      val bytes: Array[Byte]
      = "...}...?".getBytes("UTF-8")
      val chunk: Long = ByteBuffer.wrap(bytes, 0, 8).getLong

      // 3) XOR vs. mask and run the zero-byte detection
      val x = chunk ^ MASK

      val found = ((x - ONES) & ~x & HIGHBITS) != 0L
    }
  }

  


}
