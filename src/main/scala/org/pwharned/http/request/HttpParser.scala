package org.pwharned.http.request

import java.nio.ByteBuffer
class HttpParser(maxHdr: Int = 8 * 1024) extends Parser[HttpRequestView] {
  private val CR = '\r'.toByte
  private val LF = '\n'.toByte
  private val SP = ' '.toByte

  private val buf       = ByteBuffer.allocate(maxHdr)
  private var readBytes = 0       // how many bytes we’ve stuffed into buf
  private var state     = 0       // 0=request‐line, 1=headers, 2=done

  // offsets into buf
  private var mStart = 0; private var mEnd   = -1
  private var pStart = -1; private var pEnd   = -1
  private var vStart = -1; private var vEnd   = -1
  private var hStart = -1; private var hEnd   = -1

  override inline def feed(in: ByteBuffer): Unit = {
    while (in.hasRemaining && state != 2 && buf.hasRemaining) {
      val b = in.get()
      buf.put(b)
      readBytes += 1
      val pos = readBytes - 1

      state match {
        // 1) first space: end of METHOD
        case 0 if b == SP && mEnd < 0 =>
          mEnd   = pos
          pStart = pos + 1

        // 2) second space: end of PATH
        case 0 if b == SP && mEnd >= 0 && pEnd < 0 =>
          pEnd    = pos
          vStart  = pos + 1

        // 3) CRLF ends the request‐line, so record version end + start of headers
        case 0 if b == LF && buf.get(pos - 1) == CR =>
          vEnd   = pos - 1
          hStart = pos + 1
          state  = 1

        // 4) CRLFCRLF ends headers
        case 1 if b == LF
          && buf.get(pos - 1) == CR
          && buf.get(pos - 2) == LF
          && buf.get(pos - 3) == CR =>
          hEnd   = pos - 3
          state  = 2

        case _ => ()
      }
    }
  }

  override def take(): Option[HttpRequestView] = {
    if state != 2 then None
    else
      // prepare a slice of just the headers‐block
      buf.flip()
      val view = new HttpRequestView(
        buf.duplicate().limit(readBytes),
        (mStart,   mEnd -  mStart),
        (pStart,   pEnd -  pStart),
        (vStart,   vEnd -  vStart),
        (hStart,   hEnd -  hStart)
      )

      // reset for next request
      buf.clear()
      readBytes = 0
      state     = 0
      mEnd = -1; pStart = -1; pEnd = -1
      vStart = -1; vEnd = -1; hStart = -1; hEnd = -1

      Some(view)
  }
}
