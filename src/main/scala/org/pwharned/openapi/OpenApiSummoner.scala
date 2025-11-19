
package org.pwharned.openapi

import org.pwharned.http.HttpMethods.GET
import org.pwharned.http.dsl.Route
import org.pwharned.http.request.HttpRequestView
import org.pwharned.http.response.HttpResponse
import org.pwharned.io.IO

import scala.quoted.*



object OpenApiSummoner:


  /**
   * 2) Deferred dispatch: build a String => Route function once,
   *    then call it at runtime for each incoming path
   */
  inline def dispatchRoutePathFn(
                                  inline cfg: OpenApiConfig,inline Routes: Route[? <: Any]*
                                ): org.pwharned.openapi.root =
    ${ dispatchRoutePathFnImpl('cfg, 'Routes) }



  // ------------------------------------------------------------
  // Implementation of the _deferred_ dispatch macro
  // ------------------------------------------------------------
  private def dispatchRoutePathFnImpl(
                                     cfg: Expr[OpenApiConfig],
                                       RoutesExpr: Expr[Seq[Route[? <: Any]]]
                                     )(using qctx: Quotes): Expr[org.pwharned.openapi.root ] = {
    import qctx.reflect.*


    given fromExprServer: FromExpr[server] = new FromExpr[server] {
      override def unapply(x: Expr[server])(using Quotes): Option[server] = x match {
        case '{ ${ Expr(s: server) } } => Some(s)
        case _ => None
      }
    }

    given FromExpr[List[server]] = new FromExpr[List[server]] {
      override def unapply(x: Expr[List[server]])(using Quotes): Option[List[server]] = x match {
        case '{ List(${ Varargs(elems) } *) } => {
          val elemFromExpr = elems.map(e => fromExprServer.unapply(e.asExprOf[server]))
          if elemFromExpr.forall(_.isDefined) then Some(elemFromExpr.flatten.toList) else None
        }

      }
    }
    given ToExpr[root] = new ToExpr[root] {
      override def apply(x: root)(using Quotes): Expr[root] = {
        Expr(root(
          openapi = x.openapi,
          info = x.info,
          servers = x.servers,
          paths = x.paths,
          components = None
        ))
      }
    }

    val config = cfg match {
      case '{ OpenApiConfig(version = ${ Expr(version: String) }, title = ${ Expr(title: String) }, description = ${ Expr(description: Option[String]) }, servers = ${ Expr(servers: List[server]) }) } => OpenApiConfig(version, title, description, servers)
    }

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
    val data: Seq[(String, List[String], Expr[HttpRequestView => IO[HttpResponse[String]]])] = {
      RoutesExpr match
        case Varargs(es) =>
          es.map { routeExpr =>
            val term = strip(routeExpr.asTerm)
            term match

              case Apply(TypeApply(Apply(Ident("serverLogic"),
              List(Inlined(Some(Apply(Apply(Ident(method),
              List(Ident("endpoint"))),
              List(Typed(Repeated(List(Literal(StringConstant(path))),
              Inferred()), Inferred())))), Nil, _))), _),
              List(handlerFunction)) => {
                val cleanHandler = handlerFunction match {
                  case fn@Block(List(DefDef(_,
                  List(TermParamClause(List(param))),
                  Inferred(), Some(body))), Closure(_, _)) => {

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
                (method, segments, cleanHandler)

              }
              case other =>
                val treeStr = s"=== raw RoutesExpr AST ===\n${other.show(using Printer.TreeStructure)}"

                report.errorAndAbort(
                  s"Unexpected input got:\n  ${other.show}"
                )
          }

        case _ =>
          report.errorAndAbort("Routes must be provided as repeated args")
    }


    val paths = data.groupBy { x => {
      x._2
    }

    }.map(x => {
      val get = x._2.find(x => x._1 == "GET").map(x => x._2.mkString("/"))
      val post = x._2.find(x => x._1 == "POST").map(x => x._2.mkString("/"))
      val patch = x._2.find(x => x._1 == "PATCH").map(x => x._2.mkString("/"))
      val put = x._2.find(x => x._1 == "PUT").map(x => x._2.mkString("/"))
      val delete = x._2.find(x => x._1 == "DELETE").map(x => x._2.mkString("/"))

      val getOperation = new operation("", s"get_$get", List.empty, None, Map.empty, None)
      val postOperation = new operation("", s"pst_$post", List.empty, None, Map.empty, None)
      val patchOperation = new operation("", s"patch_$patch", List.empty, None, Map.empty, None)
      val putOperation = new operation("", s"put_$put", List.empty, None, Map.empty, None)
      val deleteOperation = new operation("", s"delete_$delete", List.empty, None, Map.empty, None)

      (x._1, pathItem(get = Some(getOperation), patch = Some(patchOperation), post = Some(postOperation), put = Some(putOperation), delete = Some(deleteOperation)))

    })

    // 2) summon info/license from cfg
    val infoObj = info(
      version = config.version,
      title = config.title,
      license = None,
      description = ""
    )

    Expr(root(
      openapi = "3.0.4",
      info = infoObj.copy(license = None),
      servers = config.servers,
      paths = paths.map(x => x._1.mkString("/") -> x._2),
      components = None
    ))
  }
end OpenApiSummoner