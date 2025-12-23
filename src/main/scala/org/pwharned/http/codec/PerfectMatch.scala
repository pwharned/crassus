package org.pwharned.http.codec

import java.nio.charset.StandardCharsets
import scala.quoted.*

inline def dispatchBytesMacro[T]: (String) => Int =
  ${ dispatchBytesImpl[T] }

def dispatchBytesImpl[T: Type](using Quotes): Expr[String => Int] = {
  import quotes.reflect.*

  // 1) Pull out the field names at compile time
  val fieldNames: List[String] =
    TypeRepr
      .of[T]
      .typeSymbol
      .primaryConstructor
      .paramSymss
      .flatten
      .map(_.name)

  // 2) Lift each into an Expr[String] and its index into Expr[Int]
  val nameExprs: List[Expr[String]] = fieldNames.map(Expr(_))
  val idxExprs: List[Expr[Int]] = fieldNames.indices.map(Expr(_)).toList

  val cases: List[CaseDef] = fieldNames.zipWithIndex.map {
    case (input, output) =>
      CaseDef(
        Literal(StringConstant(input)), // pattern: literal string
        None, // no guard
        Literal(IntConstant(output)).changeOwner(
          Symbol.spliceOwner
        ) // body: return the output string
      )
  } :+ CaseDef(
    Wildcard(), // wildcard pattern for fallback
    None, // no guard
    Literal(IntConstant(-1)).changeOwner(Symbol.spliceOwner) // body: return -1
  )

  '{ (input: String) =>
    ${
      val matchExpr = Match(
        '{ input }.asTerm, // Now input is in scope
        cases
      )
      matchExpr.asExprOf[Int]
    }
  }
}
