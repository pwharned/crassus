package org.pwharned.sql.derive

import generated.assets

import scala.compiletime.{constValue, erasedValue, error, summonInline}
import scala.deriving.Mirror
import org.pwharned.sql.HKD._
import org.pwharned.sql.dialect.SqlDialect

import scala.ValueOf
import scala.compiletime.ops.int.+
import scala.compiletime.ops.boolean


type Updatable[V] <: Boolean = V match
  case Option[t]       => Updatable[t]
  case PrimaryKey[?]   => false
  case GeneratedPrimaryKey[?] => false
  case _               => true

// emulate a type-level `If`
type If[C <: Boolean, Then, Else] = C match
  case true => Then
  case false => Else

// convert a boolean literal to 1 or 0
type BoolToInt[B <: Boolean] = If[B, 1, 0]
type CountUpdatable[Elems <: Tuple] <: Int = Elems match
  case EmptyTuple => 0
  case h *: t => +[BoolToInt[Updatable[h]], CountUpdatable[t]]

trait UpdateField[V]:
  def get(v: V): Option[Any]

object UpdateField:
  given skipPk[T]: UpdateField[PrimaryKey[T]] with
    def get(pk: PrimaryKey[T]) = None

  given skipOptPk[T]: UpdateField[Option[PrimaryKey[T]]] with
    def get(opt: Option[PrimaryKey[T]]) = None

  given skipGenPk[T]: UpdateField[GeneratedPrimaryKey[T]] with
    def get(pk: GeneratedPrimaryKey[T]) = None

  given skipOptGenPk[T]: UpdateField[Option[GeneratedPrimaryKey[T]]] with
    def get(opt: Option[GeneratedPrimaryKey[T]]) = None

  given optAny[T]: UpdateField[Option[T]] with
    def get(opt: Option[T]) = opt

  given plain[T]: UpdateField[T] with
    def get(v: T) = Some(v)

trait PrimaryKeyField[V]:
  def get(v: V): Option[Any]


object PrimaryKeyField:
  given pk[T]: PrimaryKeyField[PrimaryKey[T]] with
    def get(pk: PrimaryKey[T]) = Some(()) // value doesn't matter
  given genpk[T]: PrimaryKeyField[GeneratedPrimaryKey[T]] with
    def get(pk: GeneratedPrimaryKey[T]) = Some(()) // value doesn't matter
  given optGenPk[T]: PrimaryKeyField[Option[GeneratedPrimaryKey[T]]] with
    def get(opt: Option[GeneratedPrimaryKey[T]]) = Some(()) // always include

  given optPk[T]: PrimaryKeyField[Option[PrimaryKey[T]]] with
    def get(opt: Option[PrimaryKey[T]]) = Some(()) // always include

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
    inline val upCount = constValue[ CountUpdatable[m.MirroredElemTypes] ]

    inline if upCount == 0 then
      error(
        "Cannot derive SqlUpdate[] –  no updatable fields"
      )
    new SqlUpdate[T]:
      def sql(orig: T): String = {
        val name: String = constValue[m.MirroredLabel]
        val namesAndPlaceHoldesr = loop[m.MirroredElemTypes, m.MirroredElemLabels](orig, 0)
        val keys = pkeys[m.MirroredElemTypes, m.MirroredElemLabels](orig, 0).map(
          x => s"${x._1} = ${x._2}"
        ).mkString(" AND ")
        // build and then reverse so we keep original order
        val updates = namesAndPlaceHoldesr.map{
          x => s"${x._1} = ${x._2}"
        }


        dial.updateReturning( f"update  $name set ${updates.mkString(",")} where $keys ")
      }


