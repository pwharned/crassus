// src/main/scala/org/pwharned/sql/derive/PrimaryKeyParser.scala
package org.pwharned.database.macros


import org.pwharned.database.derive.PrimaryKeyFields

import scala.quoted.*
import org.pwharned.database.hkd.*

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

    // DEBUG: Show what type we're processing
    val tpeRepr = TypeRepr.of[T].dealias
    report.info(s"=== PrimaryKeyParser Debug for type: ${tpeRepr.show} ===")
    report.info(s"Type symbol: ${tpeRepr.typeSymbol.fullName}")

    val pkSym = Symbol
      .requiredClass("org.pwharned.sql.database.HKD$PrimaryKey")
    report.info(s"PrimaryKey symbol: ${pkSym.fullName}")

    def containsPK(t: TypeRepr): Boolean =
      val result = t.dealias match
        case AppliedType(tycon, args) =>
          val isDirectPK = tycon.typeSymbol.fullName.contains("PrimaryKey")
          val hasNestedPK = args.exists(containsPK)
          // DEBUG: Show type checking
          report.info(s"  Checking AppliedType: ${t.show}")
          report.info(s"    - tycon: ${tycon.show} (${tycon.typeSymbol.fullName})")
          report.info(s"    - isDirectPK: $isDirectPK")
          report.info(s"    - args: ${args.map(_.show)}")
          report.info(s"    - hasNestedPK: $hasNestedPK")
          isDirectPK || hasNestedPK
        case other =>
          report.info(s"  Checking non-AppliedType: ${other.show}")
          false
      report.info(s"  containsPK(${t.show}) = $result")
      result

    // DEBUG: Show all case fields
    val caseFields = tpeRepr.typeSymbol.caseFields
    report.info(s"Found ${caseFields.length} case fields:")
    caseFields.zipWithIndex.foreach { (fld, idx) =>
      val fieldType = tpeRepr.memberType(fld)
      report.info(s"  [$idx] ${fld.name}: ${fieldType.show}")
    }

    val pkInfos: List[(Int, TypeRepr)] =
      caseFields
        .zipWithIndex
        .filter { (fld, idx) =>
          val fieldType = tpeRepr.memberType(fld)
          val hasPK = containsPK(fieldType)
          report.info(s"Field ${fld.name} containsPK: $hasPK")
          hasPK
        }
        .flatMap { (fldSym, idx) =>
          val fieldType = tpeRepr.memberType(fldSym).dealias
          report.info(s"Processing field ${fldSym.name} at index $idx with type: ${fieldType.show}")

          fieldType match
            case AppliedType(con, List(argTpe)) =>
              report.info(s"  Matched AppliedType with single arg: ${argTpe.show}")
              Some((idx, argTpe))
            case AppliedType(con, args) =>
              report.info(s"  Matched AppliedType with ${args.length} args: ${args.map(_.show)}")
              None
            case other =>
              report.info(s"  Did not match AppliedType, got: ${other.show}")
              None
        }

    report.info(s"Final pkInfos: ${pkInfos.length} primary key fields found")
    pkInfos.foreach { (idx, tpe) =>
      report.info(s"  Index $idx: ${tpe.show}")
    }

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