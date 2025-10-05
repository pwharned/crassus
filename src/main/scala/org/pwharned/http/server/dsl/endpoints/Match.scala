package org.pwharned.http.server.dsl.endpoints

import scala.quoted.*

inline def generateMatch(inline str: String): Unit = ${ generateMatchImpl('str) }

import scala.quoted.*


def generateMatchImpl(pathExpr: Expr[String])(using Quotes): Expr[String => Unit] = {
  import quotes.reflect.*
  
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
