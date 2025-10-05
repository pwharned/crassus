package org.pwharned.http.server.dsl

import org.pwharned.http.HttpMethods.*
import org.pwharned.http.request.HttpRequestView
import org.pwharned.http.response.HttpResponse
import org.pwharned.io.IO

import scala.annotation.tailrec
import scala.quoted.*

type Path = String &Singleton

object endpoint

case class EndpointPath(val path: String,val method: HttpMethod)
object Macros:
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




  // 1) extension: get -> macro
  extension (inline ep: endpoint.type )
    inline def get(inline args: Any*): EndpointPath = ${ getImpl('args) }

  private def getImpl[P<:Path:Type](
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
  

object IntMacros {

  extension (inline x: Int)
    inline def macroA: Int =
      ${ macroAImpl('x) }

  private def macroAImpl(xExpr: Expr[Int])(using Quotes): Expr[Int] =
    '{ $xExpr + 1 }

  extension (inline y: Int)
    inline def macroB: Int =
      ${ macroBImpl('y) }

  private def macroBImpl(yExpr: Expr[Int])(using Quotes): Expr[Int] =
    '{ $yExpr * 2 }
}