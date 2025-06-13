package org.pwharned.http

import org.pwharned.json.{JsonSerializer, serialize}
import org.pwharned.route.{Http, SSE}

import java.nio.charset.StandardCharsets

/** Build a streaming HTTP body for a concrete protocol P from a DB
 * result of type R. */
trait BodyEncoder[P[_], R]:
  def apply(r: R): HttpResponse        // we return the *whole* response

object BodyEncoder:
  // Convenience summon
  inline def apply[P[_], R](using be: BodyEncoder[P, R]): BodyEncoder[P, R] = be

given jsonIteratorEncoder[A<:Product](using js: JsonSerializer[A])
  : BodyEncoder[Http, Iterator[A]] with

  def apply(rows: Iterator[A]): HttpResponse =

    /* ----------------------- 1. build a chunk iterator -------------------- */
    // every chunk is already a UTF-8 byte array ready to be sent
    val chunks: Iterator[Array[Byte]] =
      if !rows.hasNext then                        // empty result  ⇒  "[]"
        Iterator.single("[]".getBytes(StandardCharsets.UTF_8))
      else
        val jsonRows  = rows.map(_.serialize)      // Iterator[String]
        val firstElem = Iterator.single("[" + jsonRows.next())   // "[<row0>"
        val restElems = jsonRows.map("," + _)                     // ",<rowN>"
        val closing   = Iterator.single("]")                      // "]"
        (firstElem ++ restElems ++ closing).map(_.getBytes(StandardCharsets.UTF_8))

    /* ----------------------- 2. wrap into Body.Streamed ------------------- */
    val body = Body.Streamed(() =>
      if chunks.hasNext then Some(chunks.next()) else None
    )
    
    HttpResponse(
      status  = 200,
      headers = Headers(Map("Content-Type" -> "application/json")),
      body    = body
    )
given jsonArrayEncoder[A<:Product](using js: JsonSerializer[A]): BodyEncoder[Http, List[A]] with
  def apply(rows: List[A]): HttpResponse =
    val json = rows.map(_.serialize).mkString("[", ",", "]")
    HttpResponse.ok(json, Headers(Map("Content-Type" -> "application/json")))
/** Encode rows to a *continuous* SSE stream: every row is one JSON line.   */
given sseEncoder[A<:Product](using js: JsonSerializer[A]): BodyEncoder[SSE, LazyList[A]] with
  def apply(rows: LazyList[A]): HttpResponse =
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
  def apply(rows: Iterator[A]): HttpResponse =
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
