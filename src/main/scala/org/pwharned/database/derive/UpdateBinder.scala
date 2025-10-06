package org.pwharned.database.derive


import org.pwharned.database.FieldBinder
import org.pwharned.database.hkd._

import java.sql.PreparedStatement
import scala.compiletime.{erasedValue, summonInline}
import scala.deriving.Mirror

// 1) The unified typeclass
trait UpdateBinder[CC]:
  def bind(
            stmt: PreparedStatement,
            startIdx: Int,
            obj: CC
          ): Int

object UpdateBinder:

  def apply[CC](using pkb: UpdateBinder[CC]) = pkb

  // 2) Single inline-given that does it all
  inline given derive[CC <: Product](using m: Mirror.ProductOf[CC]): UpdateBinder[CC] =
    new UpdateBinder[CC]:

      def bind(
                stmt: PreparedStatement,
                startIdx: Int,
                obj:CC
              ): Int =
        // Kick off the recursive fold
        foldPK[m.MirroredElemTypes](stmt, startIdx, obj , 0)

  // 3) Private inline recursor over the case-class's element types
  private inline def foldPK[Elems <: Tuple](
                                             stmt:       PreparedStatement,
                                             idx0:       Int,
                                             rawFields:  Any,   // at runtime a Product
                                             productIdx:     Int
                                           ): Int =
    inline erasedValue[Elems] match

      // No more elements => we’re done
      case _: EmptyTuple => idx0

      // h *: t => test the head‐type, maybe bind, then recurse
      case _: (h *: t) =>
        // Extract the field value


        // Only bind when `h` <: PrimaryKey[?]
        inline erasedValue[h] match

          case _: PrimaryKey[u] => foldPK[t](stmt, idx0, rawFields, productIdx+1)
          case _: GeneratedPrimaryKey[u] => foldPK[t](stmt, idx0, rawFields, productIdx+1)
          case _ =>
            val pkValue = rawFields.asInstanceOf[Product].productElement(productIdx).asInstanceOf[h]
            val nextParam = summonInline[FieldBinder[h]].bind(stmt, idx0, pkValue)
            foldPK[t](stmt, nextParam, rawFields, productIdx + 1)




