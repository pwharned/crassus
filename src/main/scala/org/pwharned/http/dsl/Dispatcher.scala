package org.pwharned.http.dsl

import org.pwharned.http.HttpMethods.GET
import org.pwharned.http.request.HttpRequestView
import org.pwharned.http.response.HttpResponse
import org.pwharned.io.IO

import scala.quoted.*



object Dispatcher:


  /**
   * 2) Deferred dispatch: build a String => Route function once,
   *    then call it at runtime for each incoming path
   */
  inline def dispatchRoutePathFn(
                                  inline Routes: Route[? <: Any]*
                                ): String => HttpRequestView => IO[HttpResponse[?]] =
    ${ dispatchRoutePathFnImpl('Routes) }



  // ------------------------------------------------------------
  // Implementation of the _deferred_ dispatch macro
  // ------------------------------------------------------------
  private def dispatchRoutePathFnImpl(
                                       RoutesExpr: Expr[Seq[Route[? <: Any]]]
                                     )(using qctx: Quotes): Expr[String => HttpRequestView => IO[HttpResponse[?]] ] =
    import qctx.reflect.*



    def strip(term: Term): Term = term match
      case Inlined(_, _, expansion) => expansion // Don't recurse - stop at first Inlined
      case Typed(inner, _) => strip(inner)
      case Block(Nil, expr) => strip(expr)
      case other => other


    val treeStr = s"=== raw RoutesExpr AST ===\n${RoutesExpr.asTerm.show(using Printer.TreeStructure)}"
    println(treeStr)

    def substituteSymbol(body: Term, from: Symbol, to: Symbol)(using Quotes): Term = {
      object SymbolReplacer extends TreeMap {
        def transformTerm(tree: Term)(using Quotes): Term = tree match {
          case Ident(name) if tree.symbol == from =>
            Ref(to)
          case _ =>
            super.transformTerm(tree)(to)
        }
      }

      SymbolReplacer.transformTerm(body)
    }
    // 1. Unpack the repeated args
    val data: Seq[(String, List[String], Expr[HttpRequestView => IO[HttpResponse[String]]])] =
      RoutesExpr match
        case Varargs(es) =>
          es.map { routeExpr =>
            val term = strip(routeExpr.asTerm)
            term match

              case Apply(TypeApply(Apply(Ident("serverLogic"),
                    List(Inlined(Some(Apply(Apply(Ident(method),
                      List(Ident("endpoint"))),
                        List(Typed(Repeated(List(Literal(StringConstant(path))),
                          Inferred()), Inferred())))), Nil,_))),_ ),
                            List(handlerFunction)) => {
                val cleanHandler = handlerFunction match {
                  case fn@Block(List(DefDef(_,
                        List(TermParamClause(List(param))),
                          Inferred(), Some(body ))), Closure(_, _)) => {

                    val paramSym = Symbol.newVal(Symbol.spliceOwner, param.name, param.tpt.tpe, Flags.EmptyFlags, Symbol.noSymbol)
                    val paramRef = Ref(paramSym)
                    val substitutedBody = new TreeMap {
                       def transformTerm(tree: Term)(using Quotes): Term = tree match {
                        case Ident(_) if tree.symbol == paramSym =>
                          paramRef
                        case _ => super.transformTerm(tree)(paramSym)
                      }
                    }.transformTerm(body)
                    val lambdaExpr = '{
                      (x: HttpRequestView) =>
                        ${ substitutedBody(paramSym).asExprOf[IO[HttpResponse[String]]] }
                    }
                    lambdaExpr

                  }
                  
                }
                val segments = path.stripPrefix("/").split("/").toList
                (method, segments, cleanHandler )

              }
              case other =>
                val treeStr = s"=== raw RoutesExpr AST ===\n${other.show(using Printer.TreeStructure)}"
                println(treeStr)

                report.errorAndAbort(
                  s"Unexpected input got:\n  ${other.show}"
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
    def buildMatchWithFallback(
                    depth: Int,
                    entries: Seq[(String, List[String], Expr[HttpRequestView => IO[HttpResponse[String]]])],
                    segmentsExpr: Expr[Array[String]],
                    absentFallback: Expr[HttpRequestView => IO[HttpResponse[String]]]
                  )(using Quotes): Expr[HttpRequestView => IO[HttpResponse[String]]] = {

      val validEntries = entries.filter(_._2.length > depth)
      val absentSegment = "__ABSENT_SEGMENT__"
      val scrutinee: Expr[String] = '{ $segmentsExpr.lift(${ Expr(depth) }).getOrElse(${ Expr(absentSegment) }) }


      val (dynEntries, statEntries) = validEntries.partition {
        case (_, segs, _) => segs.length > depth && isDyn(segs(depth))
      }

      // compute wildcard body using dynEntries (similar to buildMatch)
      val wildcardForThisLevel: Expr[HttpRequestView => IO[HttpResponse[String]]] =
        if dynEntries.nonEmpty then
          if dynEntries.forall(_._2.size == depth + 1) then dynEntries.head._3
          else buildMatch(depth + 1, dynEntries, segmentsExpr)
        else absentFallback

      // Then build statCases as before and include case for absentSegment to call absentFallback (already done)

      val statGroups = statEntries.groupBy(_._2(depth))

      val statCases: List[CaseDef] = statGroups.toList.map { (seg, es) =>
        val body: Expr[HttpRequestView => IO[HttpResponse[String]]] =
          if es.forall(_._2.size == depth + 1) then es.head._3
          else buildMatch(depth + 1, es, segmentsExpr)

        CaseDef(Literal(StringConstant(seg)), None, body.asTerm)
      }
      val finalCases = if (dynEntries.nonEmpty) {
        // Add both absent segment case AND wildcard case for dynamic entries
        statCases :+ CaseDef(Literal(StringConstant(absentSegment)), None, absentFallback.asTerm) :+
          CaseDef(Wildcard(), None, wildcardForThisLevel.asTerm)
      } else {
        // Only absent segment case if no dynamic entries
        statCases :+ CaseDef(Literal(StringConstant(absentSegment)), None, absentFallback.asTerm)
      }

      val matchExpr: Expr[HttpRequestView => IO[HttpResponse[String]]] =
        Match(scrutinee.asTerm, finalCases).asExprOf[HttpRequestView => IO[HttpResponse[String]]]

      matchExpr
    }

    def buildMatch(
                    depth: Int,
                    entries: Seq[(String, List[String], Expr[HttpRequestView => IO[HttpResponse[String]]])],
                    segmentsExpr: Expr[Array[String]]
                  )(using Quotes): Expr[HttpRequestView => IO[HttpResponse[String]]] = {

      val validEntries = entries.filter(_._2.length > depth)
      // at top of method
      val absentSegLiteral = "__ABSENT_SEGMENT__"

      val scrutinee: Expr[String] =
        '{ $segmentsExpr.lift(${ Expr(depth) }).getOrElse(${ Expr(absentSegLiteral) }) }



      val (dynEntries, statEntries) = validEntries.partition {
        case (_, segs, _) => segs.length > depth && isDyn(segs(depth))
      }

      val statGroups = statEntries.groupBy(_._2(depth))
      // routes that end exactly at this depth (they matched up through index `depth`)
      val terminalEntriesBySegment: Map[String, Seq[Expr[HttpRequestView => IO[HttpResponse[String]]]]] =
        validEntries
          .filter(_._2.length == depth + 1) // route whose last index is `depth`
          .groupBy(_._2(depth))
          .view.mapValues(_.map(_._3)).toMap

      val wildcardBody: Expr[HttpRequestView => IO[HttpResponse[String]]] =
        if dynEntries.nonEmpty then
          if dynEntries.forall(_._2.size == depth + 1) then dynEntries.head._3
          else buildMatch(depth + 1, dynEntries, segmentsExpr)
        else '{
          (req: HttpRequestView) =>
            IO.pure(new HttpResponse(404, Seq.empty, "The server could not locate a matching resource."))
        }
      val statCases: List[CaseDef] = statGroups.toList.map { (seg, es) =>
        // body for deeper static cases (same as before)
        val innerBody: Expr[HttpRequestView => IO[HttpResponse[String]]] =
          if es.forall(_._2.size == depth + 1) then es.head._3
          else buildMatch(depth + 1, es, segmentsExpr)

        // if a terminal handler exists for this segment, use it as the absent-segment fallback
        val absentForThisSeg: Expr[HttpRequestView => IO[HttpResponse[String]]] =
          terminalEntriesBySegment.get(seg).flatMap(_.headOption).getOrElse(wildcardBody)

        // build inner match for this segment that uses absentForThisSeg for the absentSegment case
        val innerCases: List[CaseDef] = {
          // NOTE: innerBody is what should be used for non-absent deeper cases (you already build that via recursion)
          // Here we must construct the Match for seg(depth+1), but since you already call buildMatch to create that
          // innerBody, you can instead inline the absent-case by wrapping innerBody so that when it generates its
          // absentSegment case it uses absentForThisSeg; easiest approach is to pass the absent handler down as the wildcard
          // when recursing. To keep changes small, detect whether innerBody was produced by recursion or is terminal:
          if es.forall(_._2.size == depth + 1) then
            // no deeper static cases, innerBody itself is the handler for seg when next segment is absent
            // so simply create a CaseDef for seg that returns innerBody when seg matched and then
            // rely on outer match to match absentSegment -> absentForThisSeg
            List(CaseDef(Literal(StringConstant(seg)), None, innerBody.asTerm))
          else
            // we need to produce a nested match that, on absentSegment, returns absentForThisSeg.
            // implement by calling a helper that builds the deeper match but accepts a fallback for absentSegment.
            List(CaseDef(Literal(StringConstant(seg)), None, buildMatchWithFallback(depth + 1, es, segmentsExpr, absentForThisSeg).asTerm))
        }

        // return single CaseDef (above) — in the simple code path it's the CaseDef(Literal(seg),...,...)
        innerCases.head
      }

      val finalMatch =
        if (dynEntries.nonEmpty) {
          // Always add wildcard when there are dynamic entries at this depth
          CaseDef(Wildcard(), None, wildcardBody.asTerm)
        } else if (depth == 0) {
          // At root level, still need wildcard for unmatched paths
          CaseDef(Wildcard(), None, wildcardBody.asTerm)
        } else {
          // Only use absent segment case when no dynamic entries exist
          CaseDef(Literal(StringConstant(absentSegLiteral)), None, wildcardBody.asTerm)
        }


      val matchExpr: Expr[HttpRequestView => IO[HttpResponse[String]]] =
        Match(scrutinee.asTerm, statCases :+ finalMatch ).asExprOf[HttpRequestView => IO[HttpResponse[String]]]

      matchExpr
    }


  // 1) Build your ‘Term’ for the lambda
    val lambdaTerm:  Expr[String => HttpRequestView => IO[HttpResponse[String]]] = '{
    (key: String) =>
      val segments = key.stripPrefix("/").split("/")
      ${ buildMatch(0, data, 'segments) }
        } 
    // 2) Pretty-print it as Scala code
    val codeSnippet: String =
      lambdaTerm.show

    // 3) Emit it at compile time for inspection
    report.info(s"=== Generated Route Function ===\n$codeSnippet")

    // 4) Return it as an Expr
    lambdaTerm



end Dispatcher