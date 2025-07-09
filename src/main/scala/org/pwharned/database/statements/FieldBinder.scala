package org.pwharned.database.statements

import org.pwharned.database.HKD.{Nullable, PrimaryKey}
import org.pwharned.database.statements.{FieldBinder, JdbcArray}

import java.sql.{DriverManager, PreparedStatement, Types}
import java.util.UUID
import scala.Tuple.fromProductTyped
import scala.compiletime.{erasedValue, error, summonInline}
import scala.deriving.Mirror
import scala.language.implicitConversions
import scala.reflect.ClassTag

trait FieldBinder[T]:
  def bind(stmt: PreparedStatement, idx: Int, value: T): Int

object FieldBinder:

  def apply[T](using fb: FieldBinder[T]): FieldBinder[T] = fb



  inline given derivedTuple[X <: Tuple]: FieldBinder[X] =
    (
      inline erasedValue[X] match
        // Empty tuple => no-op
        case _: EmptyTuple =>
          new FieldBinder[EmptyTuple]:
            def bind(stmt: PreparedStatement, idx: Int, t: EmptyTuple): Int =
              idx

        // Non-empty tuple => bind head, then tail
        case _: (h *: t) =>
          val headFb = summonInline[FieldBinder[h]]
          val tailFb = summonInline[FieldBinder[t]]
          new FieldBinder[h *: t]:
            def bind(stmt: PreparedStatement, idx: Int, tup: h *: t): Int =
              val next = headFb.bind(stmt, idx, tup.head)
              tailFb.bind(stmt, next, tup.tail)
      ).asInstanceOf[FieldBinder[X]]

  //--------------------------------------------------------------------------
  // 2. Case-class derivation
  //--------------------------------------------------------------------------

  inline given derivedProduct[CC <: Product](using
                                             m: Mirror.ProductOf[CC],
                                             fb: FieldBinder[m.MirroredElemTypes]
                                            ): FieldBinder[CC] =
    new FieldBinder[CC]:
      def bind(stmt: PreparedStatement, idx: Int, cc: CC): Int =
        fb.bind(stmt, idx, fromProductTyped(cc))


  given FieldBinder[Int] with
    def bind(stmt: PreparedStatement, idx: Int, v: Int): Int =
      stmt.setInt(idx, v)
      idx + 1

  given [T](using ja: JdbcArray[T]): FieldBinder[List[T]] with
    def bind(stmt: PreparedStatement, idx: Int, v: List[T]): Int =
      val arr = stmt.getConnection.createArrayOf(ja.sqlType, ja.toArray(v))
      stmt.setArray(idx, arr)
      idx + 1
  given [T](using fb: FieldBinder[T]): FieldBinder[PrimaryKey[T]] with
    def bind(stmt: PreparedStatement, idx: Int, v: PrimaryKey[T]): Int =
      fb.bind(stmt,idx,v)

  given FieldBinder[Boolean] with
    def bind(stmt: PreparedStatement, idx: Int, v: Boolean): Int =
      stmt.setBoolean(idx, v)
      idx + 1
  given FieldBinder[Float] with
    def bind(stmt: PreparedStatement, idx: Int, v: Float): Int =
      stmt.setFloat(idx, v)
      idx + 1
  given FieldBinder[String] with
    def bind(stmt: PreparedStatement, idx: Int, v: String): Int =
      stmt.setString(idx, v)
      idx + 1
  given FieldBinder[java.util.UUID] with
    def bind(stmt: PreparedStatement, idx: Int, v: java.util.UUID): Int =
      stmt.setObject(idx, v)
      idx + 1
  // Option[T]: Some → bind, None → null
  given [T](using fb: FieldBinder[T]): FieldBinder[Option[T]] with
    def bind(stmt: PreparedStatement, idx: Int, opt: Option[T]): Int =
      opt match
        case Some(v) => fb.bind(stmt, idx, v)
        case None    =>
          stmt.setNull(idx, Types.VARCHAR)
          idx + 1

  // Nullable marker trait
  given [T](using fb: FieldBinder[T]): FieldBinder[Nullable[T]] with
    def bind(stmt: PreparedStatement, idx: Int, v: Nullable[T]): Int =
      if v.asInstanceOf[AnyRef] == null then
        stmt.setNull(idx, Types.VARCHAR)
        idx + 1
      else
        fb.bind(stmt, idx, v)




trait JdbcArray[T]:
  def sqlType: String
  def toArray(v: Seq[T]): Array[AnyRef]

object JdbcArray:
  given JdbcArray[String] with
    val sqlType = "varchar"
    def toArray(v: Seq[String]) = v.map(_.asInstanceOf[AnyRef]).toArray

  given JdbcArray[Int] with
    val sqlType = "integer"
    def toArray(v: Seq[Int]) =
      v.iterator
        .map(i => Integer.valueOf(i).asInstanceOf[AnyRef])
        .toArray

  given JdbcArray[UUID] with
    val sqlType = "uuid"
    def toArray(v: Seq[UUID]): Array[AnyRef] = v.map(_.asInstanceOf[AnyRef]).toArray