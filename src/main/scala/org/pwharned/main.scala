package org.pwharned
import generated.*
import org.pwharned.App.{corsHeaders, given_ExecutionContext}
import org.pwharned.server.FileReader.given
import org.pwharned.`lazy`.Lazy
import org.pwharned.http.{Body, Header, Headers, Http, HttpResponse, asPath, toPath}
import org.pwharned.http.HttpMethod.{GET, HttpMethod, POST}
import org.pwharned.http.HttpPath.HttpPath
import org.pwharned.http.HttpRequest.*
import org.pwharned.json.serialize
import org.pwharned.openapi.*
import org.pwharned.route.Router.Route
import org.pwharned.route.{Middleware, RouteRegistry, RoutingTable, httpConnection}
import org.pwharned.server.{FS, FileServer, HTTPServer, Resource}
import org.pwharned.sql.*
import org.pwharned.sql.HKD.IdHKD
import org.pwharned.sql.database.{ConnectionDetails, Database, DbTypeMapper, PostgresTypeMapper}
import org.pwharned.sql.dialect.{PostgresDialect, SqlDialect}
import org.pwharned.sql.statements.SelectStatement
import org.pwharned.utils.EnvLoader

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions


object App:
  val corsHeaders = Headers.empty
    .add(Header.AccessControlAllowOrigin, "*")
    .add(Header.AccessControlAllowMethods, "GET, POST, DELETE, PUT, PATCH, OPTIONS")
  
  




  lazy val actions_route = RouteRegistry.resourceRoutes[Http, actions]("actions")
  lazy val embeddings_route = RouteRegistry.resourceRoutes[Http, embeddings]("embeddings")

  lazy val asset_bookmarks_route = RouteRegistry.resourceRoutesNoUpdate[Http, asset_bookmarks]("asset_bookmarks")

  lazy val asset_collection_route = RouteRegistry.resourceRoutesNoUpdate[Http, asset_collection]("asset_collection")

  lazy val asset_product_route = RouteRegistry.resourceRoutesNoUpdate[Http, asset_product]("asset_product")

  lazy val asset_ratings_route = RouteRegistry.resourceRoutes[Http, asset_ratings]("asset_rating")

  lazy val asset_types_route = RouteRegistry.resourceRoutes[Http, asset_types]("asset_types")

  lazy val assets_route = RouteRegistry.resourceRoutes[Http, assets]("assets")

  lazy val attributes_route = RouteRegistry.resourceRoutes[Http, attributes]("attributes")

  lazy val  attributevalues_route = RouteRegistry.resourceRoutes[Http, attributevalues]("attributevalues")

  lazy val  brands_route = RouteRegistry.resourceRoutes[Http, brands]("brands")

  lazy val  collections_route = RouteRegistry.resourceRoutes[Http, collections]("collections")

  lazy val  comments_route = RouteRegistry.resourceRoutes[Http, comments]("comments")

  lazy val  entities_route = RouteRegistry.resourceRoutes[Http, entities]("entities")

  lazy val  entityattributes_route = RouteRegistry.resourceRoutesNoUpdate[Http, entityattributes]("entityattributes")

  lazy val  nominations_route = RouteRegistry.resourceRoutes[Http, nominations]("nominations")

  lazy val  offering_types_route = RouteRegistry.resourceRoutes[Http, offering_types]("offering_types")

  lazy val  parent_route = RouteRegistry.resourceRoutes[Http, parent]("parent")

  lazy val  practices_route = RouteRegistry.resourceRoutes[Http, practices]("practices")

  lazy val  products_route = RouteRegistry.resourceRoutes[Http, products]("products")

  lazy val  relationship_route = RouteRegistry.resourceRoutes[Http, relationship]("relationship")

  case class joined_attributes(name: Option[String], value: Option[String], id: Option[Int], aid: Option[Int])

  object joined_attributes:
    given SelectStatement[joined_attributes] with
      override def select: String = "select a.name as name, av.value as value, av.id as id, a.id as aid from attributevalues av join attributes a on a.id = av.aid;"


  lazy val  joined_attributes_route = RouteRegistry.get[Http, IdHKD[joined_attributes]]("joined_attributes")

  given dialect: SqlDialect = PostgresDialect


  given ExecutionContext = ExecutionContext.fromExecutor(Executors.newVirtualThreadPerTaskExecutor())

  given DbTypeMapper = PostgresTypeMapper


  inline def health = Route[Http, GET, Unit, String](GET, "/health/ping".asPath, (req: HttpRequest[Unit]) => Future {
    HttpResponse.ok("Ok")
  })

  inline def swagger = Route[Http, GET, Unit, String](GET, "/doc/openapi".asPath, (req: HttpRequest[Unit]) => Future {
    val source = scala.io.Source.fromFile("static/index.html")
    HttpResponse(body = Body.text(source.getLines().mkString), headers = Headers(Map("content-type" -> "text/html")))

  })




  inline def files = Route[Http, GET, Unit, String](GET, "/**".asPath, FileServer.apply[FS]("./".asPath, "./static"))
  inline def openapi = Route[Http, GET, Unit, String](GET, "/doc/openapi.json".asPath, (req: HttpRequest[Unit]) => Future {
    val source = scala.io.Source.fromFile("static/openapi.json")
    HttpResponse(body = Body.text(source.getLines().mkString), headers = Headers(Map("content-type" -> "text/html")))

  })
  

  val connectionDetails = EnvLoader.loadFromEnvFile[ConnectionDetails](".env") match {
    case Right(details) => details
    case Left(error) =>
      println(s"Error: $error")
      sys.exit(1)
  }
  lazy val insertAttributesRoute = Route[Http, POST, assetAtt, assetAtt](POST, "/api/assetAttributes".toPath, insertAssetAttributesFunction)


  given Database = Database(connectionDetails)

   val routes = (List(

    actions_route,
     embeddings_route,

    asset_bookmarks_route,

    asset_collection_route,

    asset_product_route,

    asset_ratings_route,

    asset_types_route,

    assets_route,

    attributes_route,

    attributevalues_route,

    brands_route,

    collections_route,

    comments_route,

    entities_route,

    entityattributes_route,

    nominations_route,

    offering_types_route,

    parent_route,

    practices_route,

    products_route,

    relationship_route).flatten ++ List(openapi, swagger,files, joined_attributes_route, insertAttributesRoute))
   
   routes.foreach( x=> x.withHeaders(corsHeaders))


  OpenApiBuilder.write("static/openapi.json",routes.toOpenApi.serialize)




@main
def main(): Unit =
  import org.pwharned.http.toPath
  val seg ="/api/{test}".toPath
  println(seg)


  println("building routing table")
  lazy val table  = RoutingTable.build(App.routes)
  RoutingTable.printReadable(table)

  HTTPServer.start(8080, table.asInstanceOf)