package org.pwharned.database.statements

import org.pwharned.database.HKD.{Default, Nullable, PrimaryKey}
import org.pwharned.database.dialect.SqlDialect

import scala.deriving.Mirror
import scala.compiletime.{constValue, erasedValue, summonInline}
import java.sql.PreparedStatement


trait InsertField[V]:
  /**
   * If Some(x), we emit a `?` + column, and bind x.
   * If None, we drop this column entirely.
   */
  def get(v: V): Option[Any]

object InsertField:

  // 1) New‐style Optional PK: Option[PrimaryKey[T]] → unwrap & include only if Some
  given pkOpt[T]: InsertField[Option[PrimaryKey[T]]] with
    def get(opt: Option[PrimaryKey[T]]) = opt.map(_.value)

  // 2) Direct PK: always include
  given pkAlways[T]: InsertField[PrimaryKey[T]] with
    def get(pk: PrimaryKey[T]) = Some(pk.value)

  // 3) Any other Option[T] (covers New‐Default, New‐Nullable): include only if Some
  given optAny[T]: InsertField[Option[T]] with
    def get(opt: Option[T]) = opt

  // 4) Everything else: always include
  given plain[T]: InsertField[T] with
    def get(v: T) = Some(v)




trait SqlInsert[CC]:
  /** e.g. "users" for a `case class User[...]` */
  def tableName: String
  def insertReturning(obj: CC): String

  /** e.g. ("insert into users(name) values(?)", 1) */
  def sql(cc: CC): String

  /** Bind only the included fields, in the same order as the placeholders. */
  def bind(cc: CC, stmt: PreparedStatement): Int

object SqlInsert:

  inline def apply[CC](using ins: SqlInsert[CC]): SqlInsert[CC] = ins

  inline given derived[CC <: Product](using
                                      m: Mirror.ProductOf[CC], dialect:SqlDialect
                                     ): SqlInsert[CC] =
    new SqlInsert[CC]:
      // derive "users" from "User"
      override val tableName: String =
        constValue[m.MirroredLabel].toLowerCase + "s"

      override def sql(cc: CC): String =
        val elems  = Tuple.fromProductTyped(cc)
        val labels = summonLabels[m.MirroredElemLabels]
        val cols   = extractCols[ m.MirroredElemTypes ](elems, labels)
        val ps      = List.fill(cols.size)("?").mkString(", ")
        s"insert into $tableName(${cols.mkString(", ")}) values($ps)"

      override def bind(cc: CC, stmt: PreparedStatement): Int =
        bindLoop[ m.MirroredElemTypes ](
          Tuple.fromProductTyped(cc),
          stmt,
          1
        )

      def insertReturning(obj: CC): String =
        val bareSql = sql(obj)
    
        val finalSql = dialect.insertReturning(tableName)
        finalSql


  //–– Helpers to summon field‐names from the type‐level label tuple
  private inline def summonLabels[L <: Tuple]: List[String] =
    inline erasedValue[L] match
      case _: EmptyTuple    => Nil
      case _: (h *: t) => constValue[h].toString :: summonLabels[t]

  //–– Walk the product‐tuple & label‐list, collecting only Some → column
  private inline def extractCols[Tup <: Tuple](
                                                elems: Tup,
                                                labels: List[String]
                                              ): List[String] =
    inline erasedValue[Tup] match
      case _: EmptyTuple => Nil
      case _: (h *: t)  =>
        // head value + its label, then recurse
        val cons      = elems.asInstanceOf[h *: t]
        val head = cons.head
        val tail = cons.tail
        val lab        = labels.head
        val restLabs   = labels.tail

        val fld = summonInline[InsertField[h]]
        fld.get(head) match
          case Some(_) => lab :: extractCols[t](tail, restLabs)
          case None    => extractCols[t](tail, restLabs)

  //–– Walk & bind only the Some(...) fields in order
  private inline def bindLoop[Tup <: Tuple](
                                             elems: Tup,
                                             stmt: PreparedStatement,
                                             idx0: Int
                                           ): Int =
    inline erasedValue[Tup] match
      case _: EmptyTuple => idx0
      case _: (h *: t)  =>
        val cons      = elems.asInstanceOf[h *: t]
        val head = cons.head
        val tail = cons.tail

        val fld = summonInline[InsertField[h]]
        fld.get(head) match
          case Some(_) =>
            val fb   = summonInline[FieldBinder[h]]
            val next = fb.bind(stmt, idx0, head)
            bindLoop[t](tail, stmt, next)
          case None =>
            bindLoop[t](tail, stmt, idx0)
