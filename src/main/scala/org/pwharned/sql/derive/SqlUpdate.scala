package org.pwharned.sql.derive

import scala.compiletime.{constValue, erasedValue, summonInline}
import scala.deriving.Mirror
import org.pwharned.sql.database.HKD.PrimaryKey
import org.pwharned.sql.dialect.SqlDialect

import scala.ValueOf

trait UpdateField[V]:
  def get(v: V): Option[Any]

object UpdateField:
  given skipPk[T]: UpdateField[PrimaryKey[T]] with
    def get(pk: PrimaryKey[T]) = None

  given skipOptPk[T]: UpdateField[Option[PrimaryKey[T]]] with
    def get(opt: Option[PrimaryKey[T]]) = None

  given optAny[T]: UpdateField[Option[T]] with
    def get(opt: Option[T]) = opt

  given plain[T]: UpdateField[T] with
    def get(v: T) = Some(v)

trait PrimaryKeyField[V]:
  def get(v: V): Option[Any]

object PrimaryKeyField:
  given skipPk[T]: PrimaryKeyField[PrimaryKey[T]] with
    def get(pk: PrimaryKey[T]) = Some(pk)

  given optAny[T]: PrimaryKeyField[Option[T]] with
    def get(opt: Option[T]) = None

  given plain[T]: PrimaryKeyField[T] with
    def get(v: T) = None

/**
 * Produces a List of (columnName, "?") for every field
 * your InsertField says “include me.”
 */
trait SqlUpdate[T]:
  def sql(orig: T): String

object SqlUpdate:
  inline private def pkeys[
    Elems <: Tuple, // the field‐types tuple
    Labels <: Tuple // the field‐names tuple
  ](orig: Product, idx: Int): List[(String, String)] =
    inline erasedValue[(Elems, Labels)] match 
      case _: (EmptyTuple, EmptyTuple) =>
        Nil

      case _: (h *: t, l *: ls) =>
        val value = orig.productElement(idx).asInstanceOf[h]

        val included = summonInline[PrimaryKeyField[h]].get(value)

        val colName = summonInline[ValueOf[l]].value.toString

        val tail = pkeys[t, ls](orig, idx + 1)

        included.fold(tail)(_ => (colName, s"?") :: tail)
  inline private def loop[
    Elems  <: Tuple,      // the field‐types tuple
    Labels <: Tuple       // the field‐names tuple
  ](orig: Product, idx: Int): List[(String, String)] =
    inline erasedValue[(Elems, Labels)] match
      case _: (EmptyTuple, EmptyTuple) =>
        Nil

      case _: (h *: t, l *: ls) =>
        val value = orig.productElement(idx).asInstanceOf[h]

        val included = summonInline[UpdateField[h]].get(value)

        val colName = summonInline[ValueOf[l]].value.toString

        val tail    = loop[t, ls](orig, idx + 1)

        included.fold(tail)(_ => (colName, s"?") :: tail)

  /** summon a derived instance */

  inline given derived[T <: Product](using m: Mirror.ProductOf[T], dial: SqlDialect): SqlUpdate[T] =
    new SqlUpdate[T]:
      def sql(orig: T): String = {
        val name: String = constValue[m.MirroredLabel]
        val namesAndPlaceHoldesr = loop[m.MirroredElemTypes, m.MirroredElemLabels](orig, 0).reverse
        val keys = pkeys[m.MirroredElemTypes, m.MirroredElemLabels](orig, 0).reverse.map(
          x => s"${x._1} = ${x._2}"
        ).mkString(" AND ")
        // build and then reverse so we keep original order
        val updates = namesAndPlaceHoldesr.map{
          x => s"${x._1} = ${x._2}"
        }


        dial.updateReturning( f"update  $name set ${updates.mkString(",")} where $keys ")
      }
