package org.pwharned.experiments

import scala.quoted.*

object PatternMacro:

  /**
   * Inline API: 
   *   - `scr`      is the expression to match on
   *   - `values`   are the literal strings you want to handle
   */
  inline def dispatch(inline scr: String, inline values: String*): Unit =
    ${ dispatchImpl('scr, 'values) }

  /** Macro implementation that emits `scr match { case v => println(v) … }` */
  private def dispatchImpl(
                            scrExpr:   Expr[String],
                            valuesExpr: Expr[Seq[String]]
                          )(using Quotes): Expr[Unit] =
    import quotes.reflect.*

    // 1. Unseal the Seq[String] at compile time
    val values: Seq[String] = valuesExpr.valueOrAbort

    // 2. Build a CaseDef for each string literal
    val cases: List[CaseDef] =
      values.toList.map { str =>
        val pattern = Literal(StringConstant(str))
        val rhs     = '{ println(${ Expr(str) }) }.asTerm
        CaseDef(pattern, None, rhs)
      } :+ {
        // 3. Add a wildcard case for exhaustivity
        CaseDef(Wildcard(), None, '{ () }.asTerm)
      }

    // 4. Construct the `Match(scr, cases)`
    val matchTerm: Term = Match(scrExpr.asTerm, cases)

    // 5. Return it as an Expr[Unit]
    matchTerm.asExprOf[Unit]
