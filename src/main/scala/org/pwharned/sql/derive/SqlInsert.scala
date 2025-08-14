package org.pwharned.sql.derive

import scala.compiletime.{constValue, erasedValue, summonInline}
import scala.deriving.Mirror
import org.pwharned.sql.HKD._
import org.pwharned.sql.dialect.SqlDialect

import scala.ValueOf

trait InsertField[V]:
  def get(v: V): Option[Any]

object InsertField:
  given skipPk[T]: InsertField[GeneratedPrimaryKey[T]] with
    def get(pk: GeneratedPrimaryKey[T]) = None

  given skipOptPk[T]: InsertField[Option[GeneratedPrimaryKey[T]]] with
    def get(opt: Option[GeneratedPrimaryKey[T]]) = None

  given optAny[T]: InsertField[Option[T]] with
    def get(opt: Option[T]) = opt

  given plain[T]: InsertField[T] with
    def get(v: T) = Some(v)

/**
 * Produces a List of (columnName, "?") for every field
 * your InsertField says “include me.”
 */
trait SqlInsert[T]:
  def sql(orig: T): String

object SqlInsert:

  inline private def loop[
    Elems  <: Tuple,      // the field‐types tuple
    Labels <: Tuple       // the field‐names tuple
  ](orig: Product, idx: Int): List[(String, String)] =
    inline erasedValue[(Elems, Labels)] match
      case _: (EmptyTuple, EmptyTuple) =>
        Nil

      case _: (h *: t, l *: ls) =>
        // 1) pull out the runtime value
        val value = orig.productElement(idx).asInstanceOf[h]

        // 2) decide if we include it
        val included = summonInline[InsertField[h]].get(value)

        // 3) summon the compile‐time label for this field
        val colName = summonInline[ValueOf[l]].value.toString

        // 4) recurse to the tail
        val tail    = loop[t, ls](orig, idx + 1)

        // 5) if InsertField said Some(_), prepend (colName, "?")
        included.fold(tail)(_ => tail :+ (colName -> "?"))

  inline given derived[T <: Product](using m: Mirror.ProductOf[T], dial: SqlDialect): SqlInsert[T] =
    new SqlInsert[T]:
      def sql(orig: T): String = {
        val name: String = constValue[m.MirroredLabel]

        // build and then reverse so we keep original order
        val namesAndPlaceHoldesr = loop[m.MirroredElemTypes, m.MirroredElemLabels](orig, 0)
        dial.insertReturning( f"insert into $name (${namesAndPlaceHoldesr.map(_._1).reverse.mkString(",")}) values(${namesAndPlaceHoldesr.map(_._2).mkString(",") }) ")
      }
