package org.pwharned

import generated.*
import org.pwharned.database.HKD.*
import org.pwharned.database.{ConnectionDetails, Database, DbTypeMapper, EnvLoader, FieldType, PostgresTypeMapper, SelectStatement, SqlSelect, UnionFields, UnionTypes, retrieve}
import org.pwharned.http.HttpMethod.{GET, HttpMethod, POST}
import org.pwharned.http.HttpRequest.HttpRequest
import org.pwharned.http.{BodyEncoder, HttpResponse, Segment, asPath}
import org.pwharned.route.Router.{Route, route}
import org.pwharned.route.RoutingTable.RoutingTable
import org.pwharned.route.{Http, Protocal, RouteRegistry, RoutingTable, SSE, httpConnection, httpWriter, sseConnection, sseWriter}
import org.pwharned.server.HTTPServer
import org.pwharned.http.jsonArrayEncoder
import org.pwharned.json.serialize
import org.pwharned.http.toPath
import org.pwharned.json.deserialize
import org.pwharned.http.{jsonIteratorEncoder, sseIteratorEncoder}
import org.pwharned.rpc.{RpcEndpoint, RpcRequest, RpcServer, Schema, listToCaseClass}

import java.nio.charset.StandardCharsets
import scala.concurrent.duration.DurationInt
import scala.language.implicitConversions
import java.util.concurrent.Executors
import scala.compiletime.summonInline
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.deriving.Mirror

case class assetAttributes(result: String)

object assetAttributes:
  given SelectStatement[assetAttributes] with

    override def select: String = "SELECT json_build_object(   'asset', row_to_json(a), " +
      " 'fields', COALESCE(  (SELECT json_agg(  json_build_object(  'name', attr.name,   'value', av.value))" +
      "    FROM entityattributes e JOIN attributes attr ON attr.id = e.aid JOIN attributevalues av ON av.id = e.vid WHERE e.eid = a.asset_id  GROUP BY e.eid), " +
      "    '[]'::json" +
      " )) AS result FROM   ASSETS a"

case class parent_child(child: String, Parent: String)

object parent_child:
  given SelectStatement[parent_child]:
    override def select: String = "select a.name as child, b.name as parent from attributes a join parent c on c.caid = a.id join parent p on p.paid = c.paid join attributes b on b.id = p.paid"


@main
def main(): Unit =
  

  given ExecutionContext = ExecutionContext.fromExecutor(Executors.newVirtualThreadPerTaskExecutor())
  given DbTypeMapper = PostgresTypeMapper


  //inline def r: Route[Http, GET] = route[Http,GET](GET, "/health/ping".asPath, (req: HttpRequest) => Future{ HttpResponse.ok("Ok")})
  inline def r = route[SSE,GET](GET, "/health/ping".asPath, (req: HttpRequest) => Future{ HttpResponse.ok("Ok")})
  inline def rpc = route[SSE,POST](POST, "/api/rpc".asPath, (req: HttpRequest) => Future{

    rpcServer.handle(StandardCharsets.UTF_8.decode(req.body).toString)

  })

  case class SubtractOne(args: List[Int])
  case class SubtractOneArgs(a:Int, b: Int)
  case class SubtractOneResult(r: Int)
  inline given SubtractOneEndpoint: RpcEndpoint[SubtractOneArgs, SubtractOneResult]:
    val name = "subtractOne"

    def call(p: SubtractOneArgs) = SubtractOneResult(p.a - p.b)

    inline override def decodeParams(args: List[Int| String]): Either[String, SubtractOneArgs] =
      try Right(listToCaseClass[SubtractOneArgs](args))
      catch
        case e: Throwable =>
          Left(s"bad args for SubtractOneArgs: ${e.getMessage}")
  
    override def schemaP: Schema[SubtractOneArgs] = Schema[SubtractOneArgs]

    override def schemaR: Schema[SubtractOneResult] = Schema[SubtractOneResult]


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
  
  type IdHKD[T] = [F[_]] =>> T



  inline def assetAttributeRoute = RouteRegistry.get[Http, IdHKD[assetAttributes]]
  inline def parent_child_route = RouteRegistry.get[Http, IdHKD[parent_child]]
  inline def table: RoutingTable[Segment, Protocal] = RoutingTable.build[Segment, Protocal](List(assetAttributeRoute, parent_child_route, rpc))


  HTTPServer.start(8080, table)







