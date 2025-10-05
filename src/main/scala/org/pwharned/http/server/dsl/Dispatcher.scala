package org.pwharned.http.server.dsl

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
      case Typed(Inlined(Some(callSite), bindings, inner), typeAnnotation) =>
        callSite.asInstanceOf[Term] // Convert Tree to Term
      case Typed(Inlined(None, bindings, inner), typeAnnotation) =>
        strip(inner) // No call site, use inner
      case Inlined(_, _, inner) => strip(inner)
      case Block(_, expr) => strip(expr)
      case other => other

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
                  List(Ident("endpoint"))), List(Typed(Repeated(List(Literal(StringConstant(_))),
                    Inferred()), Inferred())))), Nil, Typed(Inlined(Some(TypeIdent("Macros$")), Nil,
                      Apply(Select(Ident("EndpointPath"), "apply"),
                        List(Inlined(None, Nil, Literal(StringConstant(path))),
                          Inlined(None, Nil, Inlined(_, Nil,
                            Ident(_)))))), TypeIdent("EndpointPath"))))), List(Inferred())),
                              List(handlerFunction))=> {
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
    def buildMatch(
                    depth: Int,
                    entries: Seq[(String, List[String], Expr[HttpRequestView => IO[HttpResponse[String]]])],
                    segmentsExpr: Expr[Array[String]]
                  )(using Quotes): Expr[HttpRequestView => IO[HttpResponse[String]]] = {
      val validEntries = entries.filter(_._2.length > depth)

      val scrutinee: Expr[String] = '{ $segmentsExpr(${Expr(depth)}) }

      val (dynEntries, statEntries) = validEntries.partition {
        case (_, segs, _) => segs.length > depth && isDyn(segs(depth))
      }

      val statGroups = statEntries.groupBy(_._2(depth))

      val statCases: List[CaseDef] = statGroups.toList.map { (seg, es) =>
        val body: Expr[HttpRequestView => IO[HttpResponse[String]]] =
          if es.forall(_._2.size == depth + 1) then es.head._3
          else buildMatch(depth + 1, es, segmentsExpr)

        CaseDef(Literal(StringConstant(seg)), None, body.asTerm)
      }

      val wildcardBody: Expr[HttpRequestView => IO[HttpResponse[String]]] =
        if dynEntries.nonEmpty then
          if dynEntries.forall(_._2.size == depth + 1) then dynEntries.head._3
          else buildMatch(depth + 1, dynEntries, segmentsExpr)
        else '{
          (req: HttpRequestView) =>
            IO.pure(new HttpResponse(404, Seq.empty, "The server could not locate a matching resource."))
        }

      val matchExpr: Expr[HttpRequestView => IO[HttpResponse[String]]] =
        Match(scrutinee.asTerm, statCases :+ CaseDef(Wildcard(), None, wildcardBody.asTerm)).asExprOf[HttpRequestView => IO[HttpResponse[String]]]

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