package org.pwharned.http.request


import java.nio.ByteBuffer

/**
 * Incrementally scan input bytes, record where the request-line 
 * and headers live, but never allocate on feed(). 
 * Once complete, `take()` returns a view and resets for the next request.
 */
trait Parser[Req] {
  /** Feed raw TCP bytes into the parser. Zero allocations here. */
  @inline def feed(in: ByteBuffer): Unit

  /** Once a full request is seen, returns Some(view) and clears internal state. */
  def take(): Option[Req]
}

/** A minimal “view” over the raw buffer that lets you lazily extract method, path, headers. */
final class HttpRequestView(
                             private val buf: ByteBuffer,
                             val methodOff:  (Int,Int),
                             val pathOff:    (Int,Int),
                             val versionOff: (Int,Int),
                             val headerOff:  (Int,Int)
                           ) {
  import HttpRequestView.*

  def method: String  = slice(buf, methodOff)
  def path: String    = slice(buf, pathOff)
  def version: String = slice(buf, versionOff)

  /** Lazily parse headers into a Map only if you call .headers */
  lazy val headers: Map[String,String] = parseHeaders(buf, headerOff)
}

object HttpRequestView {
  private def slice(buf: ByteBuffer, off: (Int,Int)): String = {
    val (pos,len) = off
    val dup = buf.duplicate()
    dup.position(pos).limit(pos+len)
    val arr = new Array[Byte](len); dup.get(arr)
    new String(arr, "UTF-8")
  }

  private def parseHeaders(buf: ByteBuffer, off: (Int,Int)): Map[String,String] = {
    val (pos,len) = off
    val dup = buf.duplicate()
    dup.position(pos).limit(pos+len)
    val arr = new Array[Byte](len); dup.get(arr)
    new String(arr, "UTF-8")
      .split("\r\n")
      .map(_.split(":",2))
      .collect { case Array(k,v) => k.trim -> v.trim }
      .toMap
  }
}
