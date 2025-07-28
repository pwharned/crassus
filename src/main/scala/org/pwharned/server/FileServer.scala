package org.pwharned.server

import org.pwharned.http.HttpRequest.HttpRequest
import org.pwharned.http.{Body, Headers, HttpResponse, Segment}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import java.io.{File, InputStream}
import scala.io.Source

object FileServer {
  // Simple mime‐type lookup by file extension
  inline private def mimeTypes = Map(
    "html" -> "text/html",
    "css"  -> "text/css",
    "js"   -> "application/javascript",
    "json" -> "application/json",
    "png"  -> "image/png",
    "jpg"  -> "image/jpeg",
    "jpeg" -> "image/jpeg",
    "gif"  -> "image/gif",
    "svg"  -> "image/svg+xml"
  )

  /**
   * @param resourceRoot
   *   the classpath prefix under which static files live.
   *   e.g. "/static" if your JAR contains resources in src/main/resources/static/
   */
  inline def apply(resourceRoot: String): HttpRequest[Unit] => Future[HttpResponse[String]] = { req =>
    Future {
      // Reconstruct the raw URI the client asked for, e.g. "/static/index.js"
      val requestPath = req.path.segments.collect {
        case Segment.Static(ps)   => ps.value.toString
        case Segment.Dynamic(id)  => id.value.toString
        case Segment.WildCard(w)  => w.value.toString
      }.mkString("", "/", "")

      // Try to open it from the classloader
      Option(Source. fromResource(requestPath )) match {
        case Some(stream) =>
          // Read all bytes
          val bytes = stream.map(x => x.toByte ).toArray
          // Derive extension and lookup mime
          val ext = requestPath
            .split("\\.")
            .lastOption
            .getOrElse("")
            .toLowerCase
          val contentType = mimeTypes.getOrElse(ext, "application/octet-stream")

          // Build a 200 OK with Content-Type
          HttpResponse(
            status  = 200,
            headers = Headers(Map("Content-Type" -> contentType)),
            body    = Body.Strict(bytes)
          )

        case None =>
          // Not found in JAR
          HttpResponse(
            status  = 404,
            headers = Headers.empty,
            body    = Body.Strict(Array.emptyByteArray)
          )
      }
    }
  }
}
