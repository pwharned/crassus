package org.pwharned.openapi


import org.pwharned.route.Router.Route
import org.pwharned.http.HttpMethod.{DELETE, GET, HttpMethod, PATCH, POST, PUT}
import org.pwharned.http.Segment

import scala.compiletime.summonInline

object OpenApiBuilder:

  /** Top‐level entrypoint: pull title/version/servers from the implicit config */
  def build[F[_]](routes: List[Route[F, HttpMethod, _]])
                 (using cfg: OpenApiConfig): root =

    // 1) build the `paths` map by grouping identical paths
    val paths = routes.map({
      x=> (x.path.segments.map {
        case Segment.Static(segment) => "/" + segment.value.toString
        case Segment.Dynamic(segment) => "/" + segment.value.toString
      }.mkString("")  ,x.pathItem)
    }).toMap

    // 2) summon info/license from cfg
    val infoObj = info(
      version = cfg.version,
      title   = cfg.title,
      license = None,
      description = ""
    )

    root(
      openapi    = "3.0.4",
      info       = infoObj.copy(license = None),
      servers    = cfg.servers,
      paths      = paths,
      components = None
    )





extension [F[_]](routes: List[Route[F, HttpMethod, _]])
  /** summon your own OpenApiConfig and build the full root object */
  def toOpenApi(using cfg: OpenApiConfig): root =
    OpenApiBuilder.build(routes)
