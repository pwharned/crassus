package org.pwharned

import generated.assets
import org.pwharned.http.HttpMethod.GET
import org.pwharned.http.HttpRequest.HttpRequest
import org.pwharned.http.{Body, BodyEncoder, Headers, Http, HttpResponse, SocketWriter, asPath}
import org.pwharned.json.{JsonDeserializer, JsonSerializer, JsonString, serialize}
import org.pwharned.parse.{ParseError, QueryDeserializer}
import org.pwharned.route.Router.Route
import org.pwharned.sql.database.Connection.insert
import org.pwharned.sql.database.{Database, Row}
import org.pwharned.sql.HKD._
import org.pwharned.sql.dialect.SqlDialect
import org.pwharned.sql.statements.SelectStatement
import org.pwharned.route.{ConnectionHandler, toResponse}
import org.pwharned.sql.database.Connection.*
import org.pwharned.parse.fromQuery
import org.pwharned.sql.derive.SqlSelect

import scala.concurrent.{ExecutionContext, Future}
case class assetAttributes(result: JsonString)

object assetAttributes:
  given SelectStatement[assetAttributes] with

    override def select: String = "SELECT json_build_object(   'asset', row_to_json(a), " +
      " 'fields', COALESCE(  (SELECT json_agg(  json_build_object(  'name', attr.name,   'value', av.value))" +
      "    FROM entityattributes e JOIN attributes attr ON attr.id = e.aid JOIN attributevalues av ON av.id = e.vid WHERE e.eid = a.asset_id  GROUP BY e.eid), " +
      "    '[]'::json" +
      " )) AS result FROM   ASSETS a"

  // This given will override the derived JsonSerializer[assetAttributes].
  given JsonSerializer[assetAttributes] with
    def serialize(a: assetAttributes): String =
      // assuming JsonString is just a type alias for String,
      // or has an unwrapped `value: String` inside:
      a.result.toString

  inline def get(using db: Database, enc: BodyEncoder[Http, Iterator[assetAttributes]], ec: ExecutionContext, sw: SocketWriter[Http], ch: ConnectionHandler[Http]) = Route(GET, "/api/assetAttributes".asPath, (req: HttpRequest[Unit])  => {
    val maybeQuery: Option[String] = Option(req.path.query.value).map(_.stripMargin) match {
      case Some(value) => value match {
        case "" => None
        case _ => Some(value)
      }
      case None => None
    }


    maybeQuery match {
      case None =>
        // No query string → run basic query
        toResponse(db.withConnection(x => x.query[assetAttributes]))(enc.apply)

      case Some(raw) =>
        raw.fromQuery[Optional[assets]] match {
          case Left(error) =>
            // Malformed query → return a 400 response
            Future(HttpResponse[Iterator[assetAttributes]](500, Headers.empty, Body.text(s"Bad Request – invalid query: $error")))

          case Right(parsed) =>
            // Valid query → run parameterized query
            toResponse(db.withConnection(x => {
              val sql: String = summon[SqlSelect[assetAttributes]].select
              val where = parsed.productIterator.toSeq.zip(parsed.productElementNames).filter(x => x._1 !=None )

              val stmt = x.prepareStatement(s"$sql WHERE ${where.map(x => s"${x._2}= ?").mkString(" AND ")}")
              where.zipWithIndex.foreach{
                x => {
                  val y = x._1._1 match {
                    case Some(value) => value
                    case _ => x._1._1
                  }
                  stmt.setObject(x._2+1, y)
                }
              }
              val row = summon[Row[assetAttributes]]


              val rs = stmt.executeQuery()
              Iterator.continually(rs.next()).takeWhile(identity).map(x => row.fromRs(rs))

            }    ))(enc.apply)
        }
    }

  }         )


case class assetAtt(asset: New[assets],fields: List[assetFields] )
case class assetFields(name: String, value: String)
def insertAssetAttributesFunction(using db: Database, ec: ExecutionContext, dial: SqlDialect): HttpRequest[New[IdHKD[assetAtt]]] => Future[HttpResponse[Persisted[IdHKD[assetAtt]]]] =
  x => {
    val a: assetAtt = x.body
    db.pool.withConnection { y =>
      val newAsset = y.insert[New[assets], Persisted[assets]](a.asset)
      newAsset.foreach { na =>
        val createTable =
          """CREATE TEMPORARY TABLE temp_table (
                     asset_id text,
                     name text,
                     value text
                   );"""
        y.prepareStatement(createTable).execute()

        a.fields.foreach { f =>
          val insertStmt = y.prepareStatement("INSERT INTO temp_table (asset_id, name, value) VALUES (?, ?, ?)")
          insertStmt.setString(1, na.asset_id.value.toString)
          insertStmt.setString(2, f.name)
          insertStmt.setString(3, f.value)
          insertStmt.execute()
        }

        val finalInsert =
          s"""INSERT INTO entityattributes (vid, aid, eid)
                    SELECT attributevalues.id, attributes.id, '${na.asset_id.value}'
                    FROM attributevalues
                    JOIN attributes ON attributes.id = attributevalues.aid
                    JOIN temp_table ON temp_table.name = attributes.name AND temp_table.value = attributevalues.value"""
        y.prepareStatement(finalInsert).execute()
      }

      // Wrap in HttpResponse and return the persisted form

    }.flatMap {
      case scala.util.Success(result) => Future.successful({
        HttpResponse(
          body = Body.text(a.serialize)
        )
      })
      case scala.util.Failure(err)    => Future.failed(err)
    }

  }

