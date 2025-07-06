package org.pwharned

import generated.*
import org.pwharned.`lazy`.Lazy
import org.pwharned.database.HKD.*
import org.pwharned.database.{ConnectionDetails, Database, DbTypeMapper, EnvLoader, PostgresTypeMapper, SelectStatement, SqlSelect}
import org.pwharned.http.HttpMethod.{GET, HttpMethod, POST}
import org.pwharned.json.{JsonSerializer, JsonString, serialize}
import org.pwharned.http.HttpRequest.HttpRequest
import org.pwharned.http.{HttpResponse, Segment, asPath, textBodyEncoder}
import org.pwharned.route.Router.{Route, route}
import org.pwharned.route.{RouteRegistry, RoutingTable, httpConnection, sseConnection}
import org.pwharned.http.{Http, Protocal, SSE, SocketWriter}
import org.pwharned.server.HTTPServer
import org.pwharned.http.toPath
import org.pwharned.http.jsonIteratorEncoder
import org.pwharned.openapi.{Schema, components, schema, server, given}
import org.pwharned.rpc.{RpcEndpoint, RpcSchema, RpcServer, listToCaseClass}
import org.pwharned.http.{httpWriter, sseWriter}
import org.pwharned.openapi.toOpenApi

import java.nio.charset.StandardCharsets
import scala.language.implicitConversions
import java.util.concurrent.Executors
import scala.compiletime.summonInline
import scala.concurrent.{ExecutionContext, Future}
import scala.deriving.Mirror
import org.pwharned.route.RouteRegistry.lazily

import java.nio.ByteBuffer
case class assetAttributes(result: JsonString)

object assetAttributes:
  given SelectStatement[assetAttributes] with

    override def select: String = "SELECT json_build_object(   'asset', row_to_json(a), " +
      " 'fields', COALESCE(  (SELECT json_agg(  json_build_object(  'name', attr.name,   'value', av.value))" +
      "    FROM entityattributes e JOIN attributes attr ON attr.id = e.aid JOIN attributevalues av ON av.id = e.vid WHERE e.eid = a.asset_id  GROUP BY e.eid), " +
      "    '[]'::json" +
      " )) AS result FROM   ASSETS a"

  given JsonSerializer[assetAttributes] with
    def serialize(x: assetAttributes): String =
      // x.result is already a JsonString → emit it verbatim
      x.result.toString


case class parent_child(child: String, Parent: String)

object parent_child:
  given SelectStatement[parent_child]:
    override def select: String = "select a.name as child, b.name as parent from attributes a join parent c on c.caid = a.id join parent p on p.paid = c.paid join attributes b on b.id = p.paid"


@main
def main(): Unit =
  

  given ExecutionContext = ExecutionContext.fromExecutor(Executors.newVirtualThreadPerTaskExecutor())
  given DbTypeMapper = PostgresTypeMapper


  //inline def r: Route[Http, GET] = route[Http,GET](GET, "/health/ping".asPath, (req: HttpRequest) => Future{ HttpResponse.ok("Ok")})
  inline def r = route[SSE,GET, Unit, String](GET, "/health/ping".asPath, (req: HttpRequest[Unit]) => Future{ HttpResponse.ok("Ok")})
  inline def rpc = route[SSE,POST, ByteBuffer, String](POST, "/api/rpc".asPath, (req: HttpRequest[ByteBuffer]) => Future{

    rpcServer.handle(StandardCharsets.UTF_8.decode(req.body).toString)

  })

  case class SubtractOne(args: List[Int])
  case class SubtractOneArgs(a:Int, b: Int)
  case class SubtractOneResult(r: Int)
  inline given SubtractOneEndpoint: RpcEndpoint[SubtractOneArgs, SubtractOneResult]:
    val name = "subtractOne"

    def call(p: SubtractOneArgs): SubtractOneResult = SubtractOneResult(p.a - p.b)

    inline override def decodeParams(args: List[Int| String]): Either[String, SubtractOneArgs] =
      try Right(listToCaseClass[SubtractOneArgs](args))
      catch
        case e: Throwable =>
          Left(s"bad args for SubtractOneArgs: ${e.getMessage}")
  
    override def schemaP: RpcSchema[SubtractOneArgs] = RpcSchema[SubtractOneArgs]

    override def schemaR: RpcSchema[SubtractOneResult] = RpcSchema[SubtractOneResult]


  inline def rpcServer = new RpcServer(endpoints = List(SubtractOneEndpoint))


  // HTTP: one JSON array
  given Database.type = Database

  val connectionDetails = EnvLoader.loadFromEnvFile[ConnectionDetails](".env") match {
    case Right(details) => details
    case Left(error) =>
      println(s"Error: $error")
      sys.exit(1)
  }

  Database.createPool(connectionDetails)
  inline def assetRoutes = RouteRegistry.resourceRoutes[Http, assets]

  inline def getAssetsRoute = RouteRegistry.get[Http,assets]


  inline def assetAttributeRoute = RouteRegistry.get[Http, IdHKD[assetAttributes]]

  inline def parent_child_route = RouteRegistry.get[Http, IdHKD[parent_child]]

  val jsString: JsonString = JsonString("hello")
  val serialized = jsString.serialize
  println(serialized)
  val routes = List(assetAttributeRoute, parent_child_route, r)


  println(routes.toOpenApi.serialize)


  lazy val table  = RoutingTable.build(routes.map( x=> Lazy(() => x)))
  println(table)
  HTTPServer.start(8080, table)







