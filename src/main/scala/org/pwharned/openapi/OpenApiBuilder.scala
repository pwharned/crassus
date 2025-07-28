package org.pwharned.openapi


import org.pwharned.route.Router.Route
import org.pwharned.http.HttpMethod.{DELETE, GET, HttpMethod, PATCH, POST, PUT}
import org.pwharned.http.Segment
import org.pwharned.json.serialize
import scala.compiletime.summonInline

object OpenApiBuilder:

  /** Top‐level entrypoint: pull title/version/servers from the implicit config */
  def build[F[_]](routes: List[Route[F, HttpMethod, ?,?]])
                 (using cfg: OpenApiConfig): root =

    // 1) build the `paths` map by grouping identical paths

    val paths = routes.groupBy{ x=>
      {
        x.path.segments.map {
          case Segment.Static(segment) => "/" + segment.value.toString
          case Segment.Dynamic(segment) => "/" + segment.value.toString
          case Segment.WildCard(segment) => "/" + segment.value.toString

        }.mkString("")
      } }.map(x=> {
      val get=  x._2.find( x=> x.method==GET).flatMap(x => x.pathItem.get)
      val patch=  x._2.find( x=> x.method==PATCH).flatMap(x => x.pathItem.patch)
      val post=  x._2.find( x=> x.method==POST).flatMap(x => x.pathItem.post)
      val put=  x._2.find( x=> x.method==PUT).flatMap( x=> x.pathItem.put)
      val delete = x._2.find(x => x.method == DELETE).flatMap(x => x.pathItem.delete)

      (x._1, pathItem(get= get, patch = patch, post = post, put = put, delete = delete))

    })

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
    
  def write(p: String, api: String): Unit = {
    import java.io.PrintWriter
    println(s"Writing the openapispec to $p")
    val pw = new PrintWriter(p) // opens (or creates) the file
    try {
      pw.write(api)
    } finally {
      pw.close() // always close to flush and free resources
    }

  }
    
  





extension [F[_]](routes: List[Route[F, HttpMethod, ?,?]])
  /** summon your own OpenApiConfig and build the full root object */
  def toOpenApi(using cfg: OpenApiConfig): root =
    OpenApiBuilder.build(routes)
