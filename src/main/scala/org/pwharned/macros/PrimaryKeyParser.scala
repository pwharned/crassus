// src/main/scala/org/pwharned/sql/derive/PrimaryKeyParser.scala
package org.pwharned.sql.derive

import org.pwharned.macros.FromString

import scala.quoted.*
import org.pwharned.sql.HKD._

object PrimaryKeyParser:

  /** Summons at compile‐time a parser:
   *
   *     Seq[String] => PrimaryKeyFields[T]#Out
   *
   * where each element of the Seq is run through FromString[U] and wrapped. */
  inline def makeParser[T <: Product]: Seq[String] => PrimaryKeyFields[T]#Out =
    ${ makeParserImpl[T] }


  private def makeParserImpl[T: Type](using q: Quotes)
  : Expr[Seq[String] => PrimaryKeyFields[T]#Out] =
    import q.reflect.*

    // 1) Dealiased T and the PrimaryKey constructor symbol
    val tpeRepr       = TypeRepr.of[T].dealias
    //val pkConstructor = TypeRepr.of[PrimaryKey[?]].typeSymbol
    val pkSym = Symbol
      .requiredClass("org.pwharned.sql.database.HKD$PrimaryKey")

    // 2) Dealiased T

    def containsPK(t: TypeRepr): Boolean =
      t.dealias match
        case AppliedType(tycon, args) =>
          // if this constructor *is* PrimaryKey
          if tycon.typeSymbol.fullName.contains("PrimaryKey") then true
          else args.exists(containsPK)
        case _ =>
          false

    val pkInfos: List[(Int, TypeRepr)] =
      tpeRepr.typeSymbol
        .caseFields
        .zipWithIndex
        .filter((fld,idx) => containsPK(tpeRepr.memberType(fld)))
        .flatMap { (fldSym, idx) =>
          tpeRepr.memberType(fldSym).dealias match
            case AppliedType(con, List(argTpe)) => Some((idx, argTpe))
            case _ =>
              None
        }
    // 2) Walk its case‐fields, pick those of shape PrimaryKey[U], remember their index + U‐type

    // 5) Emit the final lambda, splicing in the tuple and casting to Out
    '{
      (ss: Seq[String]) =>
        ${ Expr.ofTupleFromSeq(pkInfos.map { (idx, argTpe) =>
          argTpe.asType match
            case '[u] =>
              val fsExpr: Expr[FromString[u]] =
                Expr.summon[FromString[u]].getOrElse {
                  report.errorAndAbort(
                    s"No FromString instance found for ${Type.show[u]}"
                  )

                }

              // This quote *will* see `ss` in scope once we splice it below
              '{ ($fsExpr.parse(ss(${ Expr(idx) }))) }
        }) }.asInstanceOf[PrimaryKeyFields[T]#Out]
    }
