package org.pwharned.database.derive

import org.pwharned.database.hkd.*
import org.pwharned.database.sql.FieldBinder

import java.sql.PreparedStatement
import scala.compiletime.{erasedValue, summonInline}
import scala.deriving.Mirror

trait PrimaryKeyBinder[CC]:
  def bind(
      stmt: PreparedStatement,
      startIdx: Int,
      pkFields: PrimaryKeyFields[CC]#Out
  ): Int

object PrimaryKeyBinder:

  def apply[CC](using pkb: PrimaryKeyBinder[CC]) = pkb

  // 2) Single inline-given that does it all
  inline given derive[CC <: Product](using
      m: Mirror.ProductOf[CC]
  ): PrimaryKeyBinder[CC] =
    new PrimaryKeyBinder[CC]:

      def bind(
          stmt: PreparedStatement,
          startIdx: Int,
          pkFields: PrimaryKeyFields[CC]#Out
      ): Int =
        // Kick off the recursive fold
        foldPK[m.MirroredElemTypes](stmt, startIdx, pkFields, 0)

  // 3) Private inline recursor over the case-class's element types
  private inline def foldPK[Elems <: Tuple](
      stmt: PreparedStatement,
      idx0: Int,
      rawFields: Any, // at runtime a Product
      productIdx: Int
  ): Int =
    inline erasedValue[Elems] match

      // No more elements => we’re done
      case _: EmptyTuple => idx0

      // h *: t => test the head‐type, maybe bind, then recurse
      case _: (h *: t) =>
        // Extract the field value

        // Only bind when `h` <: PrimaryKey[?]
        inline erasedValue[h] match
          case _: PrimaryKey[u] =>
            val pkValue = rawFields
              .asInstanceOf[Product]
              .productElement(productIdx)
              .asInstanceOf[h]
            val nextParam =
              summonInline[FieldBinder[h]].bind(stmt, idx0, pkValue)
            foldPK[t](stmt, nextParam, rawFields, productIdx + 1)
          case _: GeneratedPrimaryKey[u] =>
            val pkValue = rawFields
              .asInstanceOf[Product]
              .productElement(productIdx)
              .asInstanceOf[h]
            val nextParam =
              summonInline[FieldBinder[h]].bind(stmt, idx0, pkValue)
            foldPK[t](stmt, nextParam, rawFields, productIdx + 1)
          case _ => foldPK[t](stmt, idx0, rawFields, productIdx)

inline given updateBinder[CC <: Product](using
    m: Mirror.ProductOf[CC]
): FieldBinder[CC] = new FieldBinder[CC]:
  def bind(stmt: PreparedStatement, idx: Int, cc: CC): Int =
    bindUpdate[m.MirroredElemTypes, m.MirroredElemLabels](stmt, idx, cc, 0)

private inline def bindUpdate[
    Elems <: Tuple,
    Labels <: Tuple
](
    stmt: PreparedStatement,
    idx0: Int,
    cc: Product,
    offset: Int
): Int =
  inline erasedValue[(Elems, Labels)] match
    case _: (EmptyTuple, EmptyTuple) =>
      idx0

    case _: (h *: t, l *: ls) =>
      val value = cc.productElement(offset).asInstanceOf[h]
      val maybeDo = summonInline[UpdateField[h]].get(value)
      val idxAfter = maybeDo
        .fold(idx0)(_ => summonInline[FieldBinder[h]].bind(stmt, idx0, value))

      bindUpdate[t, ls](stmt, idxAfter, cc, offset + 1)
