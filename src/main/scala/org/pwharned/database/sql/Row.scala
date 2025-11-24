package org.pwharned.database.sql


import java.sql.ResultSet
import scala.compiletime.{constValueTuple, erasedValue, error, summonInline}
import scala.deriving.Mirror
import scala.language.implicitConversions
import org.pwharned.database.sql.Rs
// the row-mapper type class
trait Row[A]:
  def fromRs(rs: ResultSet): A

object Row:

  // 1) “inline dispatch” – runs at compile time, does the mirror & tuple unpacking
  implicit inline def derived[A](using m: Mirror.Of[A]): Row[A] =
    inline m match
      case p: Mirror.ProductOf[A] =>
        // compute field-names and readers once, at compile time
        val labels  = constValueTuple[p.MirroredElemLabels].toList.map(_.toString)
        val readers = summonReaders[p.MirroredElemTypes]
        // call out to a plain def, passing in what we just computed
        rowForProduct(labels, readers, p)
      case _ =>
        error("Row can only be derived for case-class Products")

  // 2) inline helper to summon Rs[...] for each element type  
  private inline def summonReaders[Elems <: Tuple]: List[Rs[?]] =
    inline erasedValue[Elems] match
      case _: EmptyTuple   => Nil
      case _: (h *: t)     => summonInline[Rs[h]] :: summonReaders[t]

  // 3) the **non-inline** worker: pure runtime code
  private def rowForProduct[A](labels: List[String],
                               readers: List[Rs[?]],
                               p: Mirror.ProductOf[A]
                              ): Row[A] =
    new Row[A]:
      def fromRs(rs: ResultSet): A =
        // iterate the pre-computed labels/readers at runtime
        val values = readers.zip(labels).map { case (r, col) =>
          r.asInstanceOf[Rs[Any]].read(rs, col)
        }.toArray
        p.fromProduct(Tuple.fromArray(values))

  // 4) ergonomic extension
  extension [A](rs: ResultSet)(using row: Row[A])
    def as: A = row.fromRs(rs)
