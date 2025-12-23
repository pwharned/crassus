package org.pwharned.json
import scala.quoted.*
import scala.deriving.*
import scala.compiletime.*

object JsonMacros {
  inline def matchField[T <: Product](key: String): Int =
    ${ matchFieldImpl[T]('key) }

  private def matchFieldImpl[T <: Product](
      keyExpr: Expr[String]
  )(using q: Quotes, t: Type[T]): Expr[Int] =
    import q.reflect.*
    val tr = TypeRepr.of[T]

    val sym = tr match
      case AppliedType(tycon, args) =>
        // tycon is the type constructor (like Option, Either)
        // args are the type arguments (like Person, String, Int)
        tycon.typeSymbol
      case _ =>
        tr.typeSymbol

    if !sym.isClassDef then
      report.error(s"Not a class or case class: ${tr.show}")
      return '{ -1 }

    // caseFields returns constructor params for case classes/case objects
    val fieldSyms = sym.caseFields
    if fieldSyms.isEmpty then
      // no fields
      return '{ -1 }

    val labels: List[(String, Int)] =
      fieldSyms.zipWithIndex.map { case (fs, idx) => (fs.name, idx) }.toList

    // group by length -> then by first char -> keep index
    val casesByLen: List[(Int, List[(String, Int)])] =
      labels.groupBy(_._1.length).toList

    def mkEq(label: String, idx: Int): Expr[Int] =
      '{
        if $keyExpr == ${ Expr(label) } then ${ Expr(idx) }
        else -1
      }

    // Build nested expressions: for each length produce a branch; inside it branch by first char then equals chain.
    // Helper to build equals chain that yields an Expr[Int] with fallback acc
    def equalsChain(pairs: List[(String, Int)], acc: Expr[Int]): Expr[Int] =
      pairs.foldRight(acc) { case ((lbl, idx), accExpr) =>
        '{
          if $keyExpr == ${ Expr(lbl) } then ${ Expr(idx) }
          else $accExpr
        }
      }

    // Build branch for a given length
    def branchForLen(
        len: Int,
        pairs: List[(String, Int)],
        accLen: Expr[Int]
    ): Expr[Int] =

      // group by first char
      val firstCharBuckets: List[(Char, List[(String, Int)])] =
        pairs.groupBy(_._1.charAt(0)).toList

      // build char branches
      val charBranch: Expr[Int] =
        firstCharBuckets.foldRight(accLen) { case ((ch, bucket), accChar) =>
          val eqChain = equalsChain(bucket, accChar)
          // guard nonEmpty then char check then equals chain
          '{
            if $keyExpr.nonEmpty && $keyExpr.charAt(0) == ${ Expr(ch) } then
              $eqChain
            else $accChar
          }
        }

      // guard length before char branching
      '{ if $keyExpr.length == ${ Expr(len) } then $charBranch else $accLen }

    // fold over lengths to create the full tree
    val tree: Expr[Int] =
      casesByLen.sortBy(_._1).foldRight('{ -1 }: Expr[Int]) {
        case ((len, pairs), accLen) =>
          branchForLen(len, pairs, accLen)
      }
    tree

  inline def matchLiteral(inline lit: String, input: String): Boolean =
    ${ matchLiteralImpl('lit, 'input) }

  private def matchLiteralImpl(litExpr: Expr[String], inputExpr: Expr[String])(
      using Quotes
  ): Expr[Boolean] =
    import quotes.reflect.*
    litExpr.value match
      case Some(lit) =>
        val comparisons: List[Expr[Boolean]] =
          lit.zipWithIndex.map { case (ch, idx) =>
            '{ $inputExpr.charAt(${ Expr(idx) }) == ${ Expr(ch) } }
          }.toList
        val allChecks = comparisons
          .reduceOption { (a, b) => '{ $a && $b } }
          .getOrElse('{ true })
        '{ $inputExpr.length == ${ Expr(lit.length) } && $allChecks }
      case None =>
        report.error("Literal string must be known at compile time")
        '{ false }

}
