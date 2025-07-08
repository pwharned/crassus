package org.pwharned
import generated.*
import org.pwharned.database.HKD.PrimaryKey
import org.pwharned.`lazy`.Lazy
import org.pwharned.database.statements.{PostgresDialect, SqlDialect}
import org.pwharned.database.{ConnectionDetails, Database, DbTypeMapper, EnvLoader, FieldType, PostgresTypeMapper, SelectStatement, SqlSelect, UnionFields, UnionTypes, retrieve}
import org.pwharned.http.HttpMethod.{GET, HttpMethod}
import org.pwharned.openapi.{Schema, given_Schema_String, given_Schema_Unit, schema, toOpenApi}
import org.pwharned.http.{Body, BodyEncoder, Headers, Http, HttpResponse, Protocal, SSE, Segment, asPath, httpWriter, jsonArrayEncoder, jsonIteratorEncoder, sseIteratorEncoder, sseWriter, textBodyEncoder, toPath}
import org.pwharned.http.HttpRequest.HttpRequest
import org.pwharned.route.Router.{Route, route}
import org.pwharned.route.RoutingTable.RoutingTable
import org.pwharned.route.{RouteRegistry, RoutingTable, httpConnection, sseConnection}
import org.pwharned.server.HTTPServer
import org.pwharned.database.FieldBinder.given 
import org.pwharned.json.serialize
import org.pwharned.openapi.{Schema, components, schema, server, given}

import scala.concurrent.duration.DurationInt
import scala.language.implicitConversions
import java.util.concurrent.Executors
import scala.compiletime.summonInline
import scala.concurrent.{Await, ExecutionContext, Future}



@main
def main(): Unit =
  
  given dialect: SqlDialect = PostgresDialect


  given ExecutionContext = ExecutionContext.fromExecutor(Executors.newVirtualThreadPerTaskExecutor())
  given DbTypeMapper = PostgresTypeMapper


  inline def health = route[SSE,GET, Unit, String](GET, "/health/ping".asPath, (req: HttpRequest[Unit]) => Future{ HttpResponse.ok("Ok")})

  inline def swagger = route[Http, GET, Unit, String](GET, "/doc/openapi".asPath, (req: HttpRequest[Unit]) => Future {
    val source = scala.io.Source.fromFile("static/index.html")
    HttpResponse(body = Body.text(source.getLines().mkString), headers = Headers(Map("content-type" -> "text/html")))

  })

  inline def openapi = route[Http, GET, Unit, String](GET, "/doc/openapi.json".asPath, (req: HttpRequest[Unit]) => Future {
    val source = scala.io.Source.fromFile("static/openapi.json")
    HttpResponse(body = Body.text(source.getLines().mkString), headers = Headers(Map("content-type" -> "text/html")))

  })
  given Database.type = Database

  val connectionDetails = EnvLoader.loadFromEnvFile[ConnectionDetails](".env") match {
    case Right(details) => details
    case Left(error) =>
      println(s"Error: $error")
      sys.exit(1)
  }

  Database.createPool(connectionDetails)

  lazy val actions_route = RouteRegistry.resourceRoutes[Http, actions]

  lazy val asset_bookmarks_route = RouteRegistry.resourceRoutes[Http, asset_bookmarks]

  lazy val asset_collection_route = RouteRegistry.resourceRoutes[Http, asset_collection]

  lazy val asset_product_route = RouteRegistry.resourceRoutes[Http, asset_product]

  lazy val asset_ratings_route = RouteRegistry.resourceRoutes[Http, asset_ratings]

  lazy val asset_types_route = RouteRegistry.resourceRoutes[Http, asset_types]

  lazy val assets_route = RouteRegistry.resourceRoutes[Http, assets]

  lazy val attributes_route = RouteRegistry.resourceRoutes[Http, attributes]

  lazy val attributevalues_route = RouteRegistry.resourceRoutes[Http, attributevalues]

  lazy val brands_route = RouteRegistry.resourceRoutes[Http, brands]

  lazy val collections_route = RouteRegistry.resourceRoutes[Http, collections]

  lazy val comments_route = RouteRegistry.resourceRoutes[Http, comments]

  lazy val entities_route = RouteRegistry.resourceRoutes[Http, entities]

  lazy val entityattributes_route = RouteRegistry.resourceRoutes[Http, entityattributes]

  lazy val nominations_route = RouteRegistry.resourceRoutes[Http, nominations]

  lazy val offering_types_route = RouteRegistry.resourceRoutes[Http, offering_types]

  lazy val parent_route = RouteRegistry.resourceRoutes[Http, parent]

  lazy val practices_route = RouteRegistry.resourceRoutes[Http, practices]

  lazy val products_route = RouteRegistry.resourceRoutes[Http, products]

  lazy val relationship_route = RouteRegistry.resourceRoutes[Http, relationship]


  type IdHKD[T] = [F[_]] =>> T


  given [A](using sch: Schema[A]): Schema[PrimaryKey[A]] with
    def labels = Nil

    def `type` = sch.`type`

    def toSchema = schema(`type` = `type`, items = Some(sch.toSchema))



  def  routes: List[Route[Protocal, HttpMethod, ? ,?]] = List(
    actions_route,
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
    relationship_route).flatten ++ List(openapi, swagger)




  import java.io.PrintWriter
  println("Generating OPENAPI")
  val pw = new PrintWriter("static/openapi.json") // opens (or creates) the file
  try {
    pw.write(routes.toOpenApi.serialize)
  } finally {
    pw.close() // always close to flush and free resources
  }

  println("building routing table")
  lazy val table  = RoutingTable.build(routes)

  HTTPServer.start(8080, table)