package org.pwharned.http.dsl

import org.pwharned.http.HttpMethods.*
import org.pwharned.http.request.HttpRequestView
import org.pwharned.http.response.HttpResponse
import org.pwharned.io.IO

import scala.annotation.tailrec
import scala.quoted.*

object endpoint


case class EndpointPath(val path: String,val method: HttpMethod)
object EndpointDsl:

  given HttpMethodToExpr: [T: {Type, ToExpr}] =>ToExpr[HttpMethod]:
    def apply(method: HttpMethod)(using Quotes): Expr[HttpMethod] =
      method match
        case x => '{ (${ Expr(x) }) }

  // Fix 1: Correct syntax for given ToExpr
  given ToExpr[HttpMethod] with
    def apply(method: HttpMethod)(using Quotes): Expr[HttpMethod] =
      method match
        case GET => '{ GET }
        case POST => '{ POST }
        case PUT => '{ PUT }
        case DELETE => '{ DELETE }
  // Add other HTTP methods as needed

  given FromExpr[EndpointPath] with
    def unapply(ep: Expr[EndpointPath])(using Quotes):Option[EndpointPath] =
      ep match
        case '{${Expr(EndpointPath(path, method) )}} => Some(EndpointPath(path, method))
        case _ => None

  given FromExpr[HttpMethod] with
    def unapply(ep: Expr[HttpMethod])(using Quotes): Option[HttpMethod] =
      ep match
        case '{ ${ Expr(GET) } } => Some(GET)
        case '{ ${ Expr(POST) } } => Some(POST)
        case '{ ${ Expr(PUT) } } => Some(PUT)
        case '{ ${ Expr(DELETE) } } => Some(DELETE)

        case _ => None

  inline def generateMatch(inline str: String): Unit = ${ generateMatchImpl('str) }



  def generateMatchImpl(pathExpr: Expr[String])(using Quotes): Expr[String => Unit] = {
    import quotes.reflect.* // ✅ This gives you Lambda, Symbol.spliceOwner, MethodType, report


    val lambda = Lambda(
      owner = Symbol.spliceOwner,
      tpe = MethodType(List("value"))(_ => List(TypeRepr.of[String]), _ => TypeRepr.of[Unit]),
      rhsFn = (owner, params) => {
        val valueTree = params.head
        val valueSym = valueTree.symbol // ✅ this is the correct symbol
        val segments: List[String] = pathExpr match {
          case Expr(str: String) => str.split("/").toList
          case _ => report.errorAndAbort("Expected a constant string")
        }

        // Generate case defs for head match
        val headCases: List[CaseDef] = segments.map { segment =>
          val pattern = Literal(StringConstant(segment))
          val body = '{ println(${ Expr(segment) }) }.asTerm
          CaseDef(pattern, None, body)
        }

        val defaultHeadCase = CaseDef(Wildcard(), None, '{ println("unknown") }.asTerm)
        val headMatch = Match(Ref(Symbol.newVal(Symbol.spliceOwner, "head", TypeRepr.of[String], Flags.EmptyFlags, Symbol.noSymbol)), headCases :+ defaultHeadCase)

        // Match on parts.toList


        val headSym = Symbol.newBind(Symbol.spliceOwner, "head", Flags.EmptyFlags, TypeRepr.of[String])
        val tailSym = Symbol.newBind(Symbol.spliceOwner, "tail", Flags.EmptyFlags, TypeRepr.of[List[String]])

        val listPattern = Unapply(
          fun = Ref(Symbol.requiredMethod("scala.collection.immutable.::.unapply")),
          implicits = Nil,
          patterns = List(
            Bind(headSym, Typed(Wildcard(), TypeTree.of[String])),
            Bind(tailSym, Typed(Wildcard(), TypeTree.of[List[String]]))
          )
        )

        val cases =
          List(
            CaseDef(
              listPattern,
              None,
              headMatch
            ),
            CaseDef(Wildcard(), None, '{ println("no path segments") }.asTerm)
          )

        val valueExpr = valueTree.asExprOf[String]
        val partsSym = Symbol.newVal(owner, "parts", TypeRepr.of[List[String]], Flags.EmptyFlags, Symbol.noSymbol)
        val partsExpr: Expr[List[String]] = '{
          $valueExpr.split("/").toList
        }
        val partsDef = ValDef(partsSym, Some(partsExpr.asTerm))

        val listMatch = Match(
          Ref(partsSym),
          cases = cases
        )

        Block(List(partsDef), listMatch).changeOwner(owner)
      }
    )
    val code = lambda.asExprOf[String => Unit]



    // 3) Emit it at compile time for inspection
    report.info(s"=== Generated Route Function ===\n$code")
    code

  }




  // 1) extension: get -> macro
  extension (inline ep: endpoint.type )
    inline def get(inline args: Any*): EndpointPath = ${ getImpl('args) }

  private def getImpl[P<:Type](
                                                argsExpr: Expr[Seq[Any]])(using q: Quotes): Expr[EndpointPath] =
    import q.reflect.*


    /*
    val epTerm = epExpr.asTerm
    val path: String = epTerm match {
      case Inlined(_,_,inlined) => inlined match {
        case  Apply(TypeApply(Select(Ident("Endpoint"), "apply"), List(Inferred())), List(Literal(StringConstant(s)))) => s
      }
    }
*/
    val argNames = argsExpr match
    case Varargs(es) =>
      es.map { expr =>
        expr.asTerm match {
          case Literal(StringConstant(s)) => s
          case other =>
            // Don't use .show here either!
            report.errorAndAbort("Expected string literal in get() arguments")
        }
      }
    case other =>
      other.asTerm match {
        case Literal(StringConstant(s)) => Seq(s)
        case _ =>
          report.errorAndAbort("Expected string literal argument")
      }

    val pathLiteral: Expr[String] = Expr(argNames.mkString("/"))
    val methodLiteral: Expr[HttpMethod] = Expr(GET)
    '{  EndpointPath( $pathLiteral, $methodLiteral ) }

  extension (inline ep: EndpointPath)
     inline def serverLogic[B]( logic: HttpRequestView => IO[HttpResponse[B]]   ): Route[B] = ${ serverLogicImpl[B]('logic, 'ep) }

  private def serverLogicImpl[B: Type](
                                        logicExpr: Expr[HttpRequestView => IO[HttpResponse[B]]],
                                        epExpr:  Expr[EndpointPath]
                                      )(using q: Quotes): Expr[Route[B]] =
    import q.reflect.*
  
    epExpr match
      case '{ EndpointPath(${ pathExpr: Expr[String] }, ${ methodExpr: Expr[HttpMethod] }) } =>
        '{ new Route[B]($methodExpr, $pathExpr, $logicExpr) }
  
      case other =>
        report.errorAndAbort(s"Expected a literal EndpointPath, got: $other")
  

  