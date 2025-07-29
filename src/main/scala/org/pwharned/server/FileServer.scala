package org.pwharned.server

import org.pwharned.http.HttpPath.HttpPath
import org.pwharned.http.HttpRequest.HttpRequest
import org.pwharned.http.PathSegment.PathSegment
import org.pwharned.http.{Body, Headers, HttpResponse, Segment}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import java.io.{File, InputStream}
import java.nio.file.{Files, Paths}
import scala.io.Source
import scala.util.Try

object FileServer {
  inline private def mimeTypes = Map(
    "html" -> "text/html",
    "css" -> "text/css",
    "js" -> "application/javascript",
    "json" -> "application/json",
    "png" -> "image/png",
    "jpg" -> "image/jpeg",
    "jpeg" -> "image/jpeg",
    "gif" -> "image/gif",
    "svg" -> "image/svg+xml"
  )


  def normalize(
                                   reqPath: HttpPath,
                                   mountPath: HttpPath
                                 ): List[Segment] = {

    @annotation.tailrec
    def go(remReq: List[Segment], remMount: List[Segment]): List[Segment] = remMount match {
      // no more mount segments: return all remaining request segments
      case Nil =>
        remReq

      // static segment must match exactly by value
      case Segment.Static(expected) :: tail =>
        remReq match {
          case Segment.Static(actual) :: rest if actual == expected =>
            go(rest, tail)
          case _ =>
            // prefix didn’t match → yield empty to trigger 404
            Nil
        }

      // dynamic: drop one segment regardless of value
      case Segment.Dynamic(_) :: tail =>
        go(remReq.drop(1), tail)

      // wildcard: consume the rest of the request path
      case Segment.WildCard(_) :: _ =>
        remReq
    }

    go(reqPath.segments, mountPath.segments)
  }


  inline def apply(mountPath: HttpPath, resourceRoot: String): HttpRequest[Unit] => Future[HttpResponse[String]] = { req =>
    Future {
      // Reconstruct the raw URI the client asked for, e.g. "/static/index.js"

      val relSegments = normalize(req.path, mountPath)
        .collect { case Segment.Static(ps) => ps.value }

      val requestPath = relSegments.mkString("", "/", "")


      // Try to open it from the classloader
      Try(Paths.get(resourceRoot.stripSuffix("/") + "/" + requestPath.stripPrefix("/"))).map{
        
        stream => {
          val bytes = Files.readAllBytes(stream)
          // Derive extension and lookup mime
          val ext = requestPath
            .split("\\.")
            .lastOption
            .getOrElse("")
            .toLowerCase
          val contentType = mimeTypes.getOrElse(ext, "application/octet-stream")

          // Build a 200 OK with Content-Type
          HttpResponse[String](
            status = 200,
            headers = Headers(Map("Content-Type" -> contentType)),
            body = Body.Strict(bytes)
          )
        }
      }.toOption match {
        case Some(response) => response


        case None =>
          // Not found in JAR
          HttpResponse(
            status = 404,
            headers = Headers.empty,
            body = Body.Strict("The requested resource could not be found".getBytes)
          )
      }
    }


  }
}