// org.pwharned.http.request.HttpParser
package org.pwharned.http.request

import java.nio.ByteBuffer

class HttpParser(maxHdr: Int = 8 * 1024) extends Parser[HttpRequestView] {
  private val CR = '\r'.toByte
  private val LF = '\n'.toByte
  private val SP = ' '.toByte

  private val buf = ByteBuffer.allocate(maxHdr)
  private var readBytes = 0 // how many bytes we’ve stuffed into buf
  private var state = 0 // 0=request‐line, 1=headers, 2=done

  // offsets into buf
  private var mStart = 0; private var mEnd = -1
  private var pStart = -1;
  private var pEnd = -1 // pEnd will now be the end of the *path* part
  private var qStart = -1; private var qEnd = -1 // NEW: Query parameter offsets
  private var vStart = -1; private var vEnd = -1
  private var hStart = -1; private var hEnd = -1
  private var bStart = -1; private var bEnd = -1 // Body offsets

  override inline def feed(in: ByteBuffer): Unit = {
    while (in.hasRemaining && state != 2 && buf.hasRemaining) {
      val b = in.get()
      buf.put(b)
      readBytes += 1
      val pos = readBytes - 1

      state match {
        // 1) first space: end of METHOD
        case 0 if b == SP && mEnd < 0 =>
          mEnd = pos
          pStart = pos + 1

        // 2) second space: end of REQUEST-TARGET (path + query string)
        case 0 if b == SP && mEnd >= 0 && pEnd < 0 =>
          val requestTargetEnd = pos // pos is the space, one past last char
          pEnd = requestTargetEnd

          // Now, scan the request target (from pStart to requestTargetEnd) for '?'
          var qMarkIndex = -1
          var i = pStart
          while (i <= requestTargetEnd && qMarkIndex == -1) {
            if (buf.get(i) == '?'.toByte) {
              qMarkIndex = i
            }
            i += 1
          }

          if (qMarkIndex == -1) {
            // No query string: entire request target is the path
            pEnd = requestTargetEnd
            qStart =
              requestTargetEnd + 1 // Mark query as empty (start after path)
            qEnd = requestTargetEnd // Mark query as empty (length 0)
          } else {
            // Query string present
            pEnd = qMarkIndex - 1 // Path ends before '?'
            qStart = qMarkIndex + 1 // Query starts after '?'
            qEnd =
              requestTargetEnd // Query ends at the end of the request target
          }

          vStart = pos + 1 // Version starts after the space

        // 3) CRLF ends the request‐line, so record version end + start of headers
        case 0 if b == LF && buf.get(pos - 1) == CR =>
          vEnd = pos - 1
          hStart = pos + 1
          state = 1

        // 4) CRLFCRLF ends headers
        case 1
            if b == LF
              && buf.get(pos - 1) == CR
              && buf.get(pos - 2) == LF
              && buf.get(pos - 3) == CR =>
          hEnd = pos - 3
          bStart = pos + 1 // Body starts immediately after the CRLFCRLF
          state = 2
        case _ => ()
      }
    }
    // If we've reached state 2 (headers done) and bStart was set,
    // the body's end is the end of the currently read bytes in the buffer.
    // This is a simplification; a full parser would use Content-Length.
    if (state == 2 && bStart != -1) {
      bEnd = readBytes - 1
    } else if (state == 2 && bStart == -1) {
      // Case where there's no body, but we entered state 2 (headers done).
      // This can happen if CRLFCRLF is the last thing.
      bStart = hEnd + 4 // Assume body would start after CRLFCRLF
      bEnd = bStart - 1 // Mark as empty body (length will be <= 0)
    }
  }

  override def take(): Option[HttpRequestView] = {
    if state != 2 then
      None // Must be in state 2 (headers done, body offsets determined) to take.
    else
      buf.flip() // Prepare buffer for reading
      val view = new HttpRequestView(
        buf = buf
          .duplicate()
          .limit(
            readBytes
          ), // Pass a duplicate buffer limited to actual content
        methodOff = (mStart, mEnd - mStart),
        pathOff =
          (pStart, pEnd - pStart), // NEW: This is now just the path part
        queryOff = (qStart, qEnd - qStart), // NEW: Query part offsets
        versionOff = (vStart, vEnd - vStart),
        headerOff = (hStart, hEnd - hStart),
        bodyOff = (bStart, bEnd - bStart) // Body offsets
      )

      // reset for next request
      buf.clear()
      readBytes = 0
      state = 0
      mEnd = -1; pStart = -1; pEnd = -1
      qStart = -1; qEnd = -1 // Reset query offsets
      vStart = -1; vEnd = -1; hStart = -1; hEnd = -1
      bStart = -1; bEnd = -1

      Some(view)
  }
}
