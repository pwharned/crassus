package org.pwharned.rpc
import org.pwharned.http.HttpResponse
import org.pwharned.json.{JsonDeserializer, JsonSerializer}
import org.pwharned.parse.Parser

import scala.compiletime.summonInline
import scala.deriving.Mirror


given unionParser(using
                  pa: JsonDeserializer[Int],
                  pb: JsonDeserializer[String]
                 ): JsonDeserializer[Int | String] with
  def deserialize: Parser[Int | String] = input =>
    pa.deserialize(input) match
      case Right((a, rest)) => Right((a, rest))
      case Left(_) =>
        pb.deserialize(input).map((b, rest) => (b, rest))
        
class RpcServer(endpoints: List[RpcEndpoint[?,?]]):
  private val byName = endpoints.map(e => e.name -> e).toMap

  def handle(raw: String): HttpResponse[String] =
    // 1) parse the outer RpcRequest
    summonInline[JsonDeserializer[RpcRequest]].deserialize(raw) match
      case Left(err) =>
        HttpResponse.error(s"Could not parse RPC request: $err")

      case Right(req) =>
        // 2) look up the endpoint by method name
        byName.get(req._1.method) match
          case None =>
            HttpResponse.error( s"Method not found: ${req._1.method}")

          case Some(ep) =>
            // 3) decode the params JSON into the endpoint’s P
            ep.decodeParams(req._1.params) match
              case Left(err) =>
                HttpResponse.error(s"Invalid params: $err")

              case Right(p) =>
                // 4) invoke the business logic
                val result = ep.returnSerialized(p)
                // 5) serialize the result R back into JSON
                HttpResponse.ok(result)