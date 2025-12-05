package org.pwharned.http.dsl

import org.pwharned.http.HttpMethods.*
import org.pwharned.http.request.HttpRequestView
import org.pwharned.http.response.{EntityWriter, HttpResponse}
import org.pwharned.io.IO

import scala.annotation.tailrec
import scala.quoted.*

object endpoint

case class PartialRoute(method: String, segments: List[String])


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

  extension (inline ep: endpoint.type)
    inline def get(inline parts: String*): PartialRoute =
      ${ getImpl('parts) }

  def getImpl(partsExpr: Expr[Seq[String]])(using Quotes): Expr[PartialRoute] =
    import quotes.reflect.*

    val segments = partsExpr match
      case Varargs(es) =>
        es.map {
          case Expr(s) => s
          case _ =>
            report.errorAndAbort("Path segments must be string literals")
        }.toList
      case _ =>
        report.errorAndAbort("Expected varargs of string literals")

    '{ PartialRoute("GET", ${ Expr(segments) }) }


  extension (inline pr: PartialRoute)
    inline def serverLogic[E](
                               inline logic: HttpRequestView => IO[HttpResponse[E]]
                             ): (String, String, HttpRequestView => IO[HttpResponse[E]]  ) =
      ${ serverLogicImpl[E]('pr, 'logic) }
  
  def serverLogicImpl[E: Type](
                                prExpr: Expr[PartialRoute],
                                handlerExpr: Expr[HttpRequestView => IO[HttpResponse[E]]],

                              )(using Quotes): Expr[(String, String,  HttpRequestView => IO[HttpResponse[E]]  ) ] =
    import quotes.reflect.*
  
    prExpr match
      case '{ PartialRoute($method, $segments) } =>
        val path: List[String] = segments.valueOrAbort
        val p: String = path.mkString("/")
        val pExpr = Expr(p)
        '{ (${ Expr(method.valueOrAbort) }, $pExpr, $handlerExpr) }
  
      case _ =>
        report.errorAndAbort("PartialRoute must be literal")
