package org.pwharned.sql.derive

import org.pwharned.sql.database.FieldBinder
import org.pwharned.sql.database.HKD.{Default, Nullable, PrimaryKey}
import org.pwharned.sql.dialect.SqlDialect

import java.sql.PreparedStatement
import scala.compiletime.{constValue, erasedValue, summonInline}
import scala.deriving.Mirror


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


final class DerivedSqlInsert[CC <: Product](
                                             override val tableName: String,
                                             labels: List[String],
                                             getters: List[InsertField[Any]],
                                             binders: List[FieldBinder[Any]],
                                             dialect: SqlDialect
                                           ) extends SqlInsert[CC]:
  override def sql(cc: CC): String =
    val prod = cc.asInstanceOf[Product]
    val cols = labels.zip(getters).collect {
      case (label, g) if g.get(prod.productElement(labels.indexOf(label))).isDefined =>
        label
    }
    val ps = List.fill(cols.size)("?").mkString(", ")
    s"insert into $tableName(${cols.mkString(", ")}) values($ps)"

  override def bind(cc: CC, stmt: PreparedStatement): Int =
    val prod = cc.asInstanceOf[Product]
    var idx = 1

    for i <- labels.indices do
      val v = prod.productElement(i)
      val getter = getters(i)
      getter.get(v) match
        case Some(value) =>
          idx = binders(i).bind(stmt, idx, value.asInstanceOf)
        case None => ()

    idx

  override def insertReturning(obj: CC): String =
    dialect.insertReturning(tableName)
object SqlInsert:

  inline given derived[CC <: Product](using
                                      m: Mirror.ProductOf[CC],
                                      dialect: SqlDialect
                                     ): SqlInsert[CC] =


    val tn = constValue[m.MirroredLabel].toLowerCase + "s"
    val lbs = summonLabels[m.MirroredElemLabels]
    val gs = summonGetters[m.MirroredElemTypes]
    val bs = summonBinders[m.MirroredElemTypes]

    // instantiate our single, named class
    new DerivedSqlInsert(tn, lbs, gs, bs, dialect)

  //–– INLINE helpers to build the four Lists of metadata ––

  private inline def summonLabels[L <: Tuple]: List[String] =
    inline erasedValue[L] match
      case _: EmptyTuple    => Nil
      case _: (h *: t)      => constValue[h].toString :: summonLabels[t]

  private inline def summonGetters[T <: Tuple]: List[InsertField[Any]] =
    inline erasedValue[T] match
      case _: EmptyTuple    => Nil
      case _: (h *: t)      =>
        // we know InsertField[h] exists; cast to Any for storage
        summonInline[InsertField[h]].asInstanceOf[InsertField[Any]] :: summonGetters[t]

  private inline def summonBinders[T <: Tuple]: List[FieldBinder[Any]] =
    inline erasedValue[T] match
      case _: EmptyTuple    => Nil
      case _: (h *: t)      =>
        summonInline[FieldBinder[h]].asInstanceOf[FieldBinder[Any]] :: summonBinders[t]
