package org.pwharned.database.derive


import org.pwharned.database.hkd.*
import org.pwharned.database.sql.FieldBinder

import java.sql.PreparedStatement
import scala.compiletime.{erasedValue, summonInline}
import scala.deriving.Mirror

/**
 * Binds values of a case-class (CC) to a PreparedStatement for INSERT.
 * Skips any field whose type is PrimaryKey[_].
 * Returns the “next” parameter index after binding all non-PKs.
 */
trait InsertBinder[CC]:
  def bind(
            stmt: PreparedStatement,
            startIdx: Int,
            obj: CC
          ): Int

object InsertBinder:

  def apply[CC](using ib: InsertBinder[CC]) = ib

  /** Derive an InsertBinder for any Product (i.e. case class) */
  inline given derive[CC <: Product](using m: Mirror.ProductOf[CC]): InsertBinder[CC] =
    new InsertBinder[CC]:

      def bind(
                stmt: PreparedStatement,
                startIdx: Int,
                obj: CC
              ): Int =
        // delegate to our inline recursive binder
        foldInsert[m.MirroredElemTypes](stmt, startIdx, obj, 0)

  /** Recursively walk the field-types tuple, binding non-PKs */
  private inline def foldInsert[Elems <: Tuple](
                                                 stmt:      PreparedStatement,
                                                 idx0:      Int,
                                                 raw:       Any,    // at runtime this is the Product (case-class)
                                                 prodIndex: Int
                                               ): Int =
    inline erasedValue[Elems] match
      // no more fields, return current index
      case _: EmptyTuple =>
        idx0

      // head `h` plus tail `t`
      case _: (h *: t) =>
        inline erasedValue[h] match
          // skip primary keys entirely
          case _: GeneratedPrimaryKey[u] =>
            foldInsert[t](stmt, idx0, raw, prodIndex + 1)
          case _: Option[GeneratedPrimaryKey[u]] =>
            foldInsert[t](stmt, idx0, raw, prodIndex + 1)

          // otherwise bind this field, then recurse
          case _ =>
            // extract the actual value
            val value = raw.asInstanceOf[Product]
              .productElement(prodIndex)
              .asInstanceOf[h]

            // summon a FieldBinder[h] and bind it at idx0
            val nextIdx = summonInline[FieldBinder[h]]
              .bind(stmt, idx0, value)

            // recurse on tail with updated index and product pointer
            foldInsert[t](stmt, nextIdx, raw, prodIndex + 1)
