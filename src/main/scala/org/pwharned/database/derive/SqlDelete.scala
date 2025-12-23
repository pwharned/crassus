package org.pwharned.database.derive

import scala.compiletime.{
  constValue,
  constValueTuple,
  erasedValue,
  summonInline
}
import scala.deriving.Mirror
import org.pwharned.database.hkd.*
import org.pwharned.database.sql.SqlDialect

import scala.ValueOf

trait SqlDelete[T]:
  def sql: String
  def deleteWhere(ob: T): String
trait DeleteKey[V]:
  def get(v: String): Option[String]

object DeleteKey:
  given skipPk[T]: DeleteKey[PrimaryKey[T]] with
    def get(v: String) = Some(v)

  given plain[T]: DeleteKey[T] with
    def get(v: String) = None

  given skipGenPk[T]: DeleteKey[GeneratedPrimaryKey[T]] with
    def get(v: String) = Some(v)

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

  inline given derived[T <: Product](using
      m: Mirror.ProductOf[T],
      dial: SqlDialect
  ): SqlDelete[T] =
    new SqlDelete[T]:
      def sql: String = {
        val name: String = constValue[m.MirroredLabel]
        val keys = pkeys[m.MirroredElemTypes, m.MirroredElemLabels]
          .map(x => s"${x._1} = ${x._2} ")
          .mkString(" AND ")

        val sql = dial.deleteReturning(f"delete from  $name  where $keys ")
        sql
      }

      def names: List[String] =
        constValueTuple[m.MirroredElemLabels].productIterator.toList
          .map(_.toString)

      override def deleteWhere(obj: T): String = {
        val values = obj.productIterator.toList
        // Filter out fields with None or null values
        val where = names
          .zip(values)
          .collect {
            case (name, value) if value != None => s" $name = ? "
          }
          .mkString(" and ")

        val name: String = constValue[m.MirroredLabel]

        val sql = s"delete from $name where $where  "
        sql
      }
