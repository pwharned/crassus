package org.pwharned.experiments

import scala.quoted.*

object PrintTree {
  inline def printTree[T](inline x: T): Unit = ${printTreeImpl('x)}
  def printTreeImpl[T: Type](x: Expr[T])(using qctx: Quotes): Expr[Unit] =
    import qctx.reflect.*
    println(x.asTerm.show(using Printer.TreeStructure))
    '{()}
}
