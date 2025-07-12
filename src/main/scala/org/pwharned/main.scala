package org.pwharned
import generated.*
import org.pwharned.`lazy`.Lazy
import org.pwharned.sql.database.HKD.*
import org.pwharned.sql.dialect.{PostgresDialect, SqlDialect}
import org.pwharned.sql.*
import org.pwharned.http.HttpMethod.{GET, HttpMethod}
import org.pwharned.http.HttpRequest.HttpRequest
import org.pwharned.http.*
import org.pwharned.json.serialize
import BodyEncoder.*
import org.pwharned.http.SocketWriter.*
import org.pwharned.openapi.*
import org.pwharned.route.Router.Route
import org.pwharned.route.{RouteRegistry, RoutingTable, httpConnection, sseConnection}
import org.pwharned.server.HTTPServer
import org.pwharned.sql.database.{ConnectionDetails, Database, DbTypeMapper, PostgresTypeMapper}
import org.pwharned.utils.{EnvLoader, RandomGenerator}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions

object App:

  case class assets2[F[_]] ( asset_id: F[PrimaryKey[java.util.UUID]],
                            asset_name: F[String],asset_owner: F[String],
                             asset_description: F[String],
                             asset_type: F[String],
                             asset_link: F[String],
                             created_at: F[Default[String]],
                             updated_at: F[Nullable[String]])
  lazy val assets_route = RouteRegistry.resourceRoutes[Http, assets2]("assets")
  given dialect: SqlDialect = PostgresDialect


  given ExecutionContext = ExecutionContext.fromExecutor(Executors.newVirtualThreadPerTaskExecutor())

  given DbTypeMapper = PostgresTypeMapper


  inline def health = Route[SSE, GET, Unit, String](GET, "/health/ping".asPath, (req: HttpRequest[Unit]) => Future {
    HttpResponse.ok("Ok")
  })

  inline def swagger = Route[Http, GET, Unit, String](GET, "/doc/openapi".asPath, (req: HttpRequest[Unit]) => Future {
    val source = scala.io.Source.fromFile("static/index.html")
    HttpResponse(body = Body.text(source.getLines().mkString), headers = Headers(Map("content-type" -> "text/html")))

  })

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

  given Database = Database(connectionDetails)

  def routes: List[Route[Protocal, HttpMethod, ?, ?]] = List(
    assets_route).flatten


@main
def main(): Unit =
  




  OpenApiBuilder.write("static/openapi.json",App.routes.toOpenApi.serialize)

  println("building routing table")
  lazy val table  = RoutingTable.build(App.routes)

  HTTPServer.start(8080, table)