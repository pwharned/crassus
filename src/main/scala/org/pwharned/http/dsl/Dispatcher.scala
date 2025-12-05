
package org.pwharned.http.dsl

import scala.quoted.*
import org.pwharned.http.request.HttpRequestView
import org.pwharned.http.response.{EntityWriter, HttpResponse}
import org.pwharned.io.IO

import java.nio.ByteBuffer
import java.nio.channels.SocketChannel
import scala.compiletime.summonInline

object Dispatcher:

  inline def build(inline routes: (String, String,  HttpRequestView => IO[HttpResponse[String]]  ) *)
  : (HttpRequestView, ByteBuffer, SocketChannel) => Unit =
    ${ buildImpl('routes) }


  // =============================================================
  // MAIN DISPATCH MACRO
  // =============================================================
  private def buildImpl(routesExpr: Expr[Seq[(String, String,  HttpRequestView => IO[HttpResponse[String]]  )]  ])(using Quotes)
  : Expr[(HttpRequestView, ByteBuffer, SocketChannel) => Unit] =
    import quotes.reflect.*

    // Extract all Route literal structures safely
    val extracted: List[(String, List[String], Expr[Any], Type[?] )] =
      routesExpr match
        case Varargs(rs) =>
          rs.map {
            // ✅ FIXED: Pattern match tuple components directly
            case '{ ($method: String, $segments: String, $handler: (HttpRequestView => IO[HttpResponse[e]])) } =>
              (
                method.valueOrAbort,
                segments.valueOrAbort.split("/").toList,
                handler.asExprOf[Any],
                Type.of[e]
              )
            case other =>
              report.errorAndAbort("Invalid route literal: " + other.show)
          }.toList
        case _ =>
          report.errorAndAbort("Expected varargs of Route(...)")

    // Rest of the method stays the same...
    val grouped = extracted.groupBy(_._1)

    val methodCases: List[CaseDef] =
      grouped.map { case (method, methodRoutes) =>
        val matcher = buildPathMatcher(methodRoutes)
        CaseDef(Literal(StringConstant(method)), None, matcher.asTerm)
      }.toList

    val methodDefault =
      CaseDef(
        Wildcard(), None,
        '{
          (req: HttpRequestView, buffer: ByteBuffer, channel: SocketChannel) =>
            buffer.clear();buffer.put("HTTP/1.1 405 Method Not Allowed\r\n\r\n".getBytes);buffer.flip();channel.write(buffer);()
        }.asTerm
      )

    '{
      (req: HttpRequestView, buffer: ByteBuffer, channel: SocketChannel) =>
        ${
          Match(
            '{ req.method }.asTerm,
            methodCases :+ methodDefault
          ).asExprOf[(HttpRequestView, ByteBuffer, SocketChannel) => Unit]
        }(req, buffer, channel)
    }


  // =============================================================
  // PATH MATCHER (per method)
  // =============================================================
  private def buildPathMatcher(
                                routes: List[
                                  (String, List[String],
                                    Expr[Any], Type[?]
                                    )
                                ]
                              )(using Quotes): Expr[(HttpRequestView, ByteBuffer, SocketChannel) => Unit] =
    import quotes.reflect.*

    // Convert each route into (cond: List[String] => Boolean, bodyFn)
    val cases: List[Expr[(List[String], HttpRequestView, ByteBuffer, SocketChannel) => Boolean]] =
      routes.map { case (_, segments, handlerExpr, tpe) =>

        // Build predicate: List[String] => Boolean
        val cond: Expr[List[String] => Boolean] =
          '{
            (segs: List[String]) =>
              ${
                segments.zipWithIndex.foldLeft('{ true }: Expr[Boolean]) {
                  case (acc, (seg, idx)) =>
                    '{ $acc && segs.lift(${Expr(idx)}).contains(${Expr(seg)}) }
                }
              }
          }

        // Build bodyFn: executes handler and returns true
        val body: Expr[(HttpRequestView, ByteBuffer, SocketChannel) => Unit] =
          tpe match {
            case '[e] => '{
              (req: HttpRequestView, buffer: ByteBuffer, channel: SocketChannel) =>
                val respIO = $handlerExpr.asInstanceOf[(HttpRequestView => IO[HttpResponse[String]])].apply(req)
                val resp: HttpResponse[String] = respIO.unsafeRunOptimized()
                
                try {
                  val writer: EntityWriter[String] = EntityWriter.stringWriter
                  writer.writeResponse(resp, buffer, channel)


                } catch {
                  case e: Exception => e.printStackTrace()
                }

            }
          }


        // Wrapper returns true if matched
        '{
          (segs: List[String], req: HttpRequestView, buffer: ByteBuffer, channel: SocketChannel) =>
            if $cond(segs) then
              $body(req, buffer, channel)
              true
            else
              false
        }
      }

    // Build the full matcher lambda
    '{
      (req: HttpRequestView, buffer: ByteBuffer, channel: SocketChannel) =>
        val segments = req.path.stripPrefix("/").split("/").toList

        var matched = false
        ${
          Expr.block(
            cases.map { caseFn =>
              '{
                if !matched && $caseFn(segments, req, buffer, channel) then
                  matched = true
              }
            },
            '{ () }
          )
        }

        if !matched then
          buffer.clear()
          buffer.put("HTTP/1.1 404 Not Found\r\n\r\n".getBytes)
          buffer.flip()
          channel.write(buffer)
          ()
    }
