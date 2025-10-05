package org.pwharned.http.request

import java.nio.ByteBuffer

final class HttpRequestView(
                             private val buf: ByteBuffer,
                             val methodOff:  (Int,Int),
                             val pathOff:    (Int,Int),
                             val queryOff: (Int, Int),
                             val versionOff: (Int,Int),
                             val headerOff:  (Int,Int),
                             val bodyOff: (Int, Int)
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