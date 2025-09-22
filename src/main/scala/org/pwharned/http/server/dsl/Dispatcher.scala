package org.pwharned.http.server.dsl

import scala.quoted.*



object Dispatcher:


  /**
   * 2) Deferred dispatch: build a String => Route function once,
   *    then call it at runtime for each incoming path
   */
  inline def dispatchRoutePathFn(
                                  inline Routes: Route[? <: Any]*
                                ): String => Route[? <: Any] =
    ${ dispatchRoutePathFnImpl('Routes) }



  // ------------------------------------------------------------
  // Implementation of the _deferred_ dispatch macro
  // ------------------------------------------------------------
  private def dispatchRoutePathFnImpl(
                                       RoutesExpr: Expr[Seq[Route[? <: Any]]]
                                     )(using qctx: Quotes): Expr[String => Route[? <: Any]] =
    import qctx.reflect.*

    // 1. Unpack the repeated args
    val data: Seq[(Expr[Route[? <: Any]], List[String])] =
      RoutesExpr match
        case Varargs(es) =>
          es.map { ae =>
            ae.asTerm match
              case Apply(fun @ (Select(New(_), "<init>") |
                                TypeApply(Select(New(_), "<init>"), _)),
              List(_, Literal(StringConstant(path)), _)) =>
                (ae, path.stripPrefix("/").split("/").toList)
              case other =>
                report.errorAndAbort(
                  s"Each Route must be `new Route(\"/a/b/...\", entity)`, but got:\n  ${other.show}"
                )
          }
        case _ =>
          report.errorAndAbort("Routes must be provided as repeated args")

    def isDyn(seg: String): Boolean =
      seg.startsWith("{") && seg.endsWith("}")

    /**
     * Recursively build a nested `Match` on segment `depth`, using the
     * provided `keyExpr` as the path input.
     */
    def buildMatch(
                    depth:   Int,
                    entries: Seq[(Expr[Route[? <: Any]], List[String])],
                    keyExpr: Expr[String]
                  ): Term =

      // scrutinee: segment at `depth` from the runtime `key`
      val segTerm: Term =
        '{
          val arr = $keyExpr.stripPrefix("/").split("/")
          arr.apply(${ Expr(depth) })
        }.asTerm

      // partition exact vs dyn
      val (dynEntries, statEntries) = entries.partition { case (_, segs) =>
        isDyn(segs(depth))
      }

      val statGroups = statEntries.groupBy(_._2(depth))

      val statCases: List[CaseDef] = statGroups.toList.map { (seg, es) =>
        val body =
          if es.forall(_._2.size == depth + 1) then
            es.head._1.asTerm
          else
            buildMatch(depth + 1, es, keyExpr)
        CaseDef(Literal(StringConstant(seg)), None, body)
      }

      val wildcardBody =
        if dynEntries.nonEmpty then
          if dynEntries.forall(_._2.size == depth + 1) then
            dynEntries.head._1.asTerm
          else
            buildMatch(depth + 1, dynEntries, keyExpr)
        else
          '{ throw new MatchError($keyExpr) }.asTerm

      val wildcardCase = CaseDef(Wildcard(), None, wildcardBody)

      Match(segTerm, statCases :+ wildcardCase)

    // 2. Emit a lambda (key: String) => nestedMatch(key)
    '{
      (key: String) =>
        ${
          // here’s the only change: convert the Term into an Expr
          buildMatch(0, data, '{ key }).asExprOf[Route[? <: Any]]
        }
    }
end Dispatcher