package org.pwharned.server

import org.pwharned.http.HttpPath.HttpPath
import org.pwharned.http.HttpRequest.HttpRequest
import org.pwharned.http.PathSegment.PathSegment
import org.pwharned.http.{Body, Headers, HttpResponse, Segment}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import java.io.{ByteArrayInputStream, File, InputStream}
import java.nio.file.{Files, Paths}
import scala.io.Source
import scala.util.Try


sealed trait FileSystem

sealed trait FS extends FileSystem

sealed trait Resource extends FileSystem

trait FileReader[F <: FileSystem] {
  def readFile(path: String): Option[Array[Byte]]
}


object FileReader:

  given FileReader[FS] with {
    def readFile(path: String): Option[Array[Byte]] = {
      val filePath = Paths.get(path)
      if (Files.exists(filePath)) Some(Files.readAllBytes(filePath))
      else None
    }
  }

  given FileReader[Resource] with {
    def readFile(path: String): Option[Array[Byte]] = {
      val sourceOpt = Try(Source.fromResource(path.stripPrefix("/"))).toOption
      sourceOpt.map { source =>
        val content = source.getLines().mkString("\n")
        source.close()
        content.getBytes(java.nio.charset.StandardCharsets.UTF_8)
      }
    }
  }
end FileReader


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
            // prefix didn't match → yield empty to trigger 404
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

  private def shouldUseFallback(requestPath: String, fallbackFile: Option[String]): Boolean = {
    fallbackFile.isDefined && {
      val ext = requestPath.split("\\.").lastOption.getOrElse("").toLowerCase
      // Only use fallback for requests that look like routes (no file extension or html extension)
      // Don't use fallback for assets like CSS, JS, images, etc.
      ext.isEmpty || ext == "html"
    }
  }

  private def serveFallback[F <: FileSystem](
                                              resourceRoot: String,
                                              fallbackFile: String
                                            )(using reader: FileReader[F]): Option[HttpResponse[String]] = {
    val fallbackPath = resourceRoot.stripSuffix("/") + "/" + fallbackFile.stripPrefix("/")
    reader.readFile(fallbackPath).map { bytes =>
      HttpResponse[String](
        status = 200,
        headers = Headers(Map("Content-Type" -> "text/html")),
        body = Body.Strict(bytes)
      )
    }
  }

  // Original method without client-side routing support
  def apply[F <: FileSystem](mountPath: HttpPath, resourceRoot: String)(using reader: FileReader[F]): HttpRequest[Unit] => Future[HttpResponse[String]] = {
    apply(mountPath, resourceRoot, None)
  }

  // New method with optional client-side routing support
  def apply[F <: FileSystem](
                              mountPath: HttpPath,
                              resourceRoot: String,
                              fallbackFile: Option[String]
                            )(using reader: FileReader[F]): HttpRequest[Unit] => Future[HttpResponse[String]] = { req =>
    Future {
      val relSegments = normalize(req.path, mountPath)
        .collect { case Segment.Static(ps) => ps.value }

      val requestPath = relSegments.mkString("/", "/", "")
      val fullPath = resourceRoot.stripSuffix("/") + requestPath

      reader.readFile(fullPath) match {
        case Some(bytes) =>
          val ext = requestPath.split("\\.").lastOption.getOrElse("").toLowerCase
          val contentType = mimeTypes.getOrElse(ext, "application/octet-stream")

          HttpResponse[String](
            status = 200,
            headers = Headers(Map("Content-Type" -> contentType)),
            body = Body.Strict(bytes)
          )

        case None =>
          // Try fallback file for client-side routing
          if (shouldUseFallback(requestPath, fallbackFile)) {
            fallbackFile.flatMap(fallback => serveFallback(resourceRoot, fallback)) match {
              case Some(response) => response
              case None =>
                // Fallback file itself not found
                HttpResponse(
                  status = 404,
                  headers = Headers.empty,
                  body = Body.Strict(s"Neither requested resource nor fallback file could be found".getBytes)
                )
            }
          } else {
            // Standard 404 for asset files or when no fallback is configured
            HttpResponse(
              status = 404,
              headers = Headers.empty,
              body = Body.Strict("The requested resource could not be found".getBytes)
            )
          }
      }
    }
  }

  // Convenience method specifically for SPAs with index.html fallback
  def spa[F <: FileSystem](mountPath: HttpPath, resourceRoot: String)(using reader: FileReader[F]): HttpRequest[Unit] => Future[HttpResponse[String]] = {
    apply(mountPath, resourceRoot, Some("index.html"))
  }
}
