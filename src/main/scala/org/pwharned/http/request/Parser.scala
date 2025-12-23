package org.pwharned.http.request

import java.nio.ByteBuffer

/** Incrementally scan input bytes, record where the request-line and headers
  * live, but never allocate on feed(). Once complete, `take()` returns a view
  * and resets for the next request.
  */
trait Parser[Req] {

  /** Feed raw TCP bytes into the parser. Zero allocations here. */
  @inline def feed(in: ByteBuffer): Unit

  /** Once a full request is seen, returns Some(view) and clears internal state.
    */
  def take(): Option[Req]
}

/** A minimal “view” over the raw buffer that lets you lazily extract method,
  * path, headers.
  */
