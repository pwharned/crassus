package org.pwharned.http

import org.pwharned.json.{JsonSerializer, serialize}
import org.pwharned.http.{Http, SSE}

import java.nio.charset.StandardCharsets

/** Build a streaming HTTP body for a concrete protocol P from a DB
 * result of type R. */
trait BodyEncoder[P[_], R]:
  def apply(r: R): HttpResponse[R]        // we return the *whole* response

object BodyEncoder:
  // Convenience summon
  inline def apply[P[_], R](using be: BodyEncoder[P, R]): BodyEncoder[P, R] = be


  given jsonIteratorEncoder[A <: Product](using js: JsonSerializer[A])
  : BodyEncoder[Http, Iterator[A]] with
  
    def apply(rows: Iterator[A]): HttpResponse[Iterator[A]] =
      val chunks =
        if !rows.hasNext then Iterator.single("[]".getBytes)
        else
          val jr       = rows.map(_.serialize)
          val first    = Iterator.single(("[" + jr.next()).getBytes)
          val middles  = jr.map("," + _).map(_.getBytes)
          val closing  = Iterator.single("]".getBytes)
          first ++ middles ++ closing
  
      val body = Body.Streamed(() =>
        if chunks.hasNext then Some(chunks.next()) else None
      )
  
      HttpResponse(
        status  = 200,
        headers = Headers(Map("Content-Type" -> "application/json")),
        body    = body
      )
  given jsonArrayEncoder[A <: Product](using js: JsonSerializer[A])
  : BodyEncoder[Http, List[A]] with
  
    def apply(rows: List[A]): HttpResponse[List[A]] =
      HttpResponse.ok(rows, Headers(Map("Content-Type" -> "application/json")))
  
  given textBodyEncoder
  : BodyEncoder[Http, String ] with
  
    def apply(response: String): HttpResponse[String] =
      HttpResponse(
        status  = 200,
        headers =  Headers(Map("Content-Type" -> "text/plain")),
        body    = Body.text(response)
      )
  
  /** Encode rows to a *continuous* SSE stream: every row is one JSON line.   */
  given sseEncoder[A<:Product](using js: JsonSerializer[A]): BodyEncoder[SSE, LazyList[A]] with
    def apply(rows: LazyList[A]): HttpResponse[LazyList[A]] =
      // build a pull-based function for Body.Streamed
      def nextChunk(it: Iterator[A])(): Option[Array[Byte]] =
        if it.hasNext then
          val data = it.next().serialize + "\n\n"
          Some(data.getBytes("UTF-8"))
        else None
  
      val body = Body.Streamed(nextChunk(rows.iterator))
      HttpResponse(
        headers = Headers(Map(
          "Content-Type"  -> "text/event-stream",
          "Cache-Control" -> "no-cache",
          "Connection"    -> "keep-alive"
        )),
        body    = body
      )
  given sseIteratorEncoder[A<:Product](using js: JsonSerializer[A]): BodyEncoder[SSE, Iterator[A]] with
    def apply(rows: Iterator[A]): HttpResponse[Iterator[A]] =
      // build a pull-based function for Body.Streamed
      def nextChunk(it: Iterator[A])(): Option[Array[Byte]] =
        if it.hasNext then
          val data = it.next().serialize + "\n\n"
          Some(data.getBytes("UTF-8"))
        else None
  
      val body = Body.Streamed(nextChunk(rows))
      HttpResponse(
        headers = Headers(Map(
          "Content-Type"  -> "text/event-stream",
          "Cache-Control" -> "no-cache",
          "Connection"    -> "keep-alive"
        )),
        body    = body
      )
