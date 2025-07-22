package org.pwharned.sql.derive

import scala.compiletime.{constValue, erasedValue, summonInline}
import scala.deriving.Mirror
import org.pwharned.sql.database.HKD.PrimaryKey
import org.pwharned.sql.dialect.SqlDialect

import scala.ValueOf

trait SqlDelete[T]:
  def sql: String

trait DeleteKey[V]:
  def get(v: String): Option[String]

object DeleteKey:
  given skipPk[T]: DeleteKey[PrimaryKey[T]] with
    def get(v: String) = Some(v)

  given plain[T]: DeleteKey[T] with
    def get(v: String) = None


object SqlDelete:
  inline private def pkeys[
    Elems <: Tuple, // the field‐types tuple
    Labels <: Tuple // the field‐names tuple
  ]: List[(String, String)] =
    inline erasedValue[(Elems, Labels)] match
      case _: (EmptyTuple, EmptyTuple) =>
        Nil

      case _: (h *: t, l *: ls) =>


        val colName = summonInline[ValueOf[l]].value.toString
        val included = summonInline[DeleteKey[h]].get(colName)


        val tail = pkeys[t, ls]
        included.fold(tail)(_ => (colName, s"?") :: tail)


  inline given derived[T <: Product](using m: Mirror.ProductOf[T], dial: SqlDialect): SqlDelete[T] =
    new SqlDelete[T]:
      def sql: String = {
        val name: String = constValue[m.MirroredLabel]
        val keys = pkeys[m.MirroredElemTypes, m.MirroredElemLabels].map(
          x => s"${x._1} = ${x._2} "
        ).mkString(" AND ")
        println(keys)
        // build and then reverse so we keep original order


        dial.updateReturning( f"delete from  $name  where $keys ")
      }
