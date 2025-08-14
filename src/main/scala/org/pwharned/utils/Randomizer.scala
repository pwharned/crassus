package org.pwharned.utils

import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonInline}
import org.pwharned.utils.RandomValue
import org.pwharned.sql.HKD._

/**
 * A little typeclass whose single method is inline,
 * so calls to it will be inlined & the tuple‐match eliminated.
 */
trait Randomizer[T]:
  def randomize(orig: T): T

object Randomizer:

  /**
   * Top-level inline recursion.
   * Peels off one field at a time from the Product,
   * preserving PrimaryKey[_] and Option[PrimaryKey[_]].
   */
  inline private def loop[Elems <: Tuple](orig: Product, idx: Int): Elems =
    inline erasedValue[Elems] match
      // no more fields
      case _: EmptyTuple =>
        EmptyTuple.asInstanceOf[Elems]

      // bind head, then tail
      case _: (h *: t) =>
        val old = orig.productElement(idx).asInstanceOf[h]

        // preserve keys, regenerate everything else
        val newHead: h =
          inline erasedValue[h] match
            case _: PrimaryKey[?]         => old
            case _: Option[PrimaryKey[?]] => old
            case _                        => summonInline[RandomValue[h]].generate

        val tail: t = loop[t](orig, idx + 1)
        (newHead *: tail).asInstanceOf[Elems]

  /** Summon helper */
  def apply[T](using r: Randomizer[T]): Randomizer[T] = r

  /**
   * Derive a Randomizer for any case-class T.
   * Reassembles T from a fresh tuple of field values.
   */
  inline given derived[T <: Product](using m: Mirror.ProductOf[T]): Randomizer[T] =
    new Randomizer[T]:
      def randomize(orig: T): T =
        // build a fresh tuple of randomized/preserved fields
        val elems: m.MirroredElemTypes = loop[m.MirroredElemTypes](orig, 0)

        // reconstruct the case class
        m.fromProduct(elems)
