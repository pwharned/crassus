package org.pwharned.http

import org.pwharned.codec.Codec
import org.pwharned.http.server.TypedHttpServer
import org.pwharned.io.IO

import java.net.*
import java.nio.ByteBuffer
import java.nio.channels.*
import java.nio.charset.StandardCharsets
import scala.collection.mutable
import scala.compiletime.{erasedValue, summonInline}
import scala.deriving.*
import scala.util.{Failure, Success, Try}

// ============================================================================
// CORE TYPE SYSTEM
// ============================================================================

object HttpTypes:
  // Zero-cost byte operations with compile-time safety
  opaque type ByteSlice = (ByteBuffer, Int, Int)

  object ByteSlice:
    inline def apply(buffer: ByteBuffer, start: Int, end: Int): ByteSlice =
      (buffer, start, end)

    extension (slice: ByteSlice)
      inline def buffer: ByteBuffer = slice._1
      inline def start: Int = slice._2
      inline def end: Int = slice._3
      inline def length: Int = slice._3 - slice._2
      inline def isEmpty: Boolean = length == 0

      def toBytes: Array[Byte] =
        val bytes = Array.ofDim[Byte](length)
        val pos = buffer.position()
        buffer.position(start)
        buffer.get(bytes, 0, length)
        buffer.position(pos)
        bytes

      def toString(charset: java.nio.charset.Charset = StandardCharsets.UTF_8): String =
        new String(toBytes, charset)

      inline def charAt(index: Int): Byte =
        if index >= length then throw IndexOutOfBoundsException()
        buffer.get(start + index)

      def slice(from: Int, to: Int): ByteSlice =
        ByteSlice(buffer, start + from, start + to.min(length))

  // Type-safe HTTP methods

  // Type-safe header names
  opaque type HeaderName = String

  object HeaderName:
    val ContentType: HeaderName = "content-type"
    val ContentLength: HeaderName = "content-length"
    val TransferEncoding: HeaderName = "transfer-encoding"
    val Connection: HeaderName = "connection"
    val Host: HeaderName = "host"
    val UserAgent: HeaderName = "user-agent"
    val Accept: HeaderName = "accept"
    val Authorization: HeaderName = "authorization"
    val CacheControl: HeaderName = "cache-control"

    def apply(name: String): HeaderName = name.toLowerCase

    extension (header: HeaderName)
      inline def value: String = header
      inline def matches(other: String): Boolean = header.equalsIgnoreCase(other)

  // Type-safe paths
  opaque type HttpPath = String

  object HttpPath:
    def apply(path: String): HttpPath =
      if path.startsWith("/") then path else s"/$path"

    extension (path: HttpPath)
      inline def value: String = path
      inline def segments: Array[String] = path.split("/").filter(_.nonEmpty)
      inline def hasQueryParams: Boolean = path.contains("?")
      inline def pathOnly: String =
        val queryIndex = path.indexOf("?")
        if queryIndex == -1 then path else path.substring(0, queryIndex)
      inline def queryString: Option[String] =
        val queryIndex = path.indexOf("?")
        if queryIndex == -1 then None else Some(path.substring(queryIndex + 1))




