package org.pwharned.http.dsl
import scala.quoted._
object Inspect {

  inline def inspect[T](inline expr: T): String = ${ inspectImpl('expr) }
  def inspectImpl[T: Type](expr: Expr[T])(using q: Quotes): Expr[String] = {
    import q.reflect._
    expr.asTerm match {
      case Inlined(_, _, ValDef(name, tpt, rhsOpt)) => Expr(s"${name}")
      case other => Expr(s"Got: ${other.show}")
    }
  }
}
