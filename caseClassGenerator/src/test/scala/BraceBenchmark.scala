import java.nio.ByteBuffer

import java.nio.{ByteBuffer, ByteOrder}

object BraceScanByteBuffer {

  // Bit-trick constants
  val ONES: Long     = 0x0101010101010101L
  val MASK: Long     = 0x7D7D7D7D7D7D7D7DL
  val HIGHBITS: Long = 0x8080808080808080L

  // 1) Build a big Array[Byte] with random printable ASCII + ~1% '}'s
  val size = 200 //* 1024 * 1024
  private val rnd = new scala.util.Random(0)
  private val raw = {
    val a = Array.fill[Byte](size)((rnd.nextInt(94) + 32).toByte)
    for (_ <- 0 until size / 100) a(rnd.nextInt(size)) = '}'.toByte
    a
  }

  // 2) Wrap it once in a little-endian ByteBuffer
  val buf: ByteBuffer =
    ByteBuffer.wrap(raw)
      .order(ByteOrder.LITTLE_ENDIAN)

  // 3) Naive 8-byte grouped scan via ByteBuffer.get(pos)
  def naive8(): Unit = {
    var i   = 0
    var cnt = 0
    var pos = 0
    val limit = buf.limit() ;
    while (i < limit) {
      // eight separate get() calls
      val d0 = buf.get(i  );

      if ((d0) == '}') {
        cnt += 1
        pos=i;
      }
      i += 1
    }
    // keep cnt alive
    if (cnt < 0) println("impossible")
  }

  // 4) Bit-trick 8-byte scan via ByteBuffer.getLong(idx)
  def bitTrick8(): Unit = {
    var i   = 0
    var agg = 0L
    var pos = 0;
    val limit = buf.limit() - 8
    while (i <= limit) {
      val chunk = buf.getLong(i)
      val x     = chunk ^ MASK
      val mask  = (x - ONES) & ~x & HIGHBITS
      agg |= mask
      if (mask != 0L) {
        val bitPos    = java.lang.Long.numberOfTrailingZeros(mask)
        val byteIndex = bitPos >>> 3
        pos = i + byteIndex         // absolute index in `data`
        i = pos+1;
      }else {
        i += 8
      }
    }
    // check final mask to keep it alive
    if ((agg & HIGHBITS) == 0L)  println("no braces!")
  }

  def main(args: Array[String]): Unit = {
    // Warm up JIT
    for (_ <- 0 until 5) {
      naive8()
      bitTrick8()
    }

    // Time naive8()
    val rounds = 10000000
    val t0 = System.nanoTime()
   for (_ <- 0 until rounds)  {
      naive8()

    }
    val t1 = System.nanoTime()

    // Time bitTrick8()
    val t2 = System.nanoTime()
    for (_ <- 0 until rounds)  {
      bitTrick8()

    }
    val t3 = System.nanoTime()

    val dtNaive    = (t1 - t0) / 1e6
    val dtBitTrick = (t3 - t2) / 1e6

    println(f"naive8():    $dtNaive%.1f ms")
    println(f"bitTrick8(): $dtBitTrick%.1f ms")
    println(f"Speedup:     ${dtNaive / dtBitTrick}%.2fx")
  }
}
