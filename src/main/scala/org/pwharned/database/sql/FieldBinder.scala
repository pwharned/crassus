package org.pwharned.database.sql

import org.postgresql.core.ParameterList
import org.postgresql.util.PGobject
import org.pwharned.database.hkd.*
import org.pwharned.json.JsonString

import java.sql.{DriverManager, PreparedStatement, Types}
import java.util.UUID
import scala.Tuple.fromProductTyped
import scala.annotation.tailrec
import scala.compiletime.{erasedValue, error, summonInline}
import scala.deriving.Mirror
import scala.language.implicitConversions
import scala.reflect.ClassTag
import java.time.ZoneId

trait FieldBinder[T]:
  def sqlType: Int
  def bind(stmt: PreparedStatement, idx: Int, value: T): Int

object FieldBinder:

  def apply[T](using fb: FieldBinder[T]): FieldBinder[T] = fb

  inline given derivedTuple[X <: Tuple]: FieldBinder[X] =
    (
      inline erasedValue[X] match
        // Empty tuple => no-op
        case _: EmptyTuple =>
          new FieldBinder[EmptyTuple]:
            def sqlType: Int = -1
            def bind(stmt: PreparedStatement, idx: Int, t: EmptyTuple): Int =
              idx

        // Non-empty tuple => bind head, then tail
        case _: (h *: t) =>
          val headFb = summonInline[FieldBinder[h]]
          val tailFb = summonInline[FieldBinder[t]]
          new FieldBinder[h *: t]:
            def sqlType: Int = -1
            def bind(stmt: PreparedStatement, idx: Int, tup: h *: t): Int =
              val next = headFb.bind(stmt, idx, tup.head)
              tailFb.bind(stmt, next, tup.tail)
    ).asInstanceOf[FieldBinder[X]]

  // --------------------------------------------------------------------------
  // 2. Case-class derivation
  // --------------------------------------------------------------------------

  // –– recursive helper over an element‐type tuple

  private inline def bindProduct[Types <: Tuple](
      stmt: PreparedStatement,
      idx0: Int,
      cc: Product,
      offset: Int
  ): Int =
    inline erasedValue[Types] match
      // no more fields
      case _: EmptyTuple =>
        idx0

      // bind head, then tail
      case _: (h *: t) =>
        val fbH = summonInline[FieldBinder[h]]
        val headValue = cc.productElement(offset).asInstanceOf[h]
        val nextIdx = fbH.bind(stmt, idx0, headValue)
        bindProduct[t](stmt, nextIdx, cc, offset + 1)

  // –– single derivedProduct that walks each element directly
  inline given derivedProduct[CC <: Product](using
      m: Mirror.ProductOf[CC]
  ): FieldBinder[CC] =
    new FieldBinder[CC]:
      def sqlType: Int = 1
      def bind(
          stmt: PreparedStatement,
          idx: Int,
          cc: CC
      ): Int =
        // start recursion at element‐index 0
        bindProduct[m.MirroredElemTypes](stmt, idx, cc, 0)

  given FieldBinder[Int] with
    def sqlType: Int = java.sql.Types.INTEGER
    def bind(stmt: PreparedStatement, idx: Int, v: Int): Int =
      stmt.setInt(idx, v)
      idx + 1

  given [T](using ja: JdbcArray[T]): FieldBinder[List[T]] with
    def sqlType: Int = java.sql.Types.ARRAY

    def bind(stmt: PreparedStatement, idx: Int, v: List[T]): Int =
      val arr = stmt.getConnection.createArrayOf(ja.sqlType, ja.toArray(v))
      stmt.setArray(idx, arr)
      idx + 1
  given [T](using fb: FieldBinder[T]): FieldBinder[PrimaryKey[T]] with
    def sqlType: Int = fb.sqlType
    def bind(stmt: PreparedStatement, idx: Int, v: PrimaryKey[T]): Int =
      fb.bind(stmt, idx, v.value)

  given [T](using fb: FieldBinder[T]): FieldBinder[GeneratedPrimaryKey[T]] with
    def sqlType: Int = fb.sqlType
    def bind(
        stmt: PreparedStatement,
        idx: Int,
        v: GeneratedPrimaryKey[T]
    ): Int =
      fb.bind(stmt, idx, v.value)

  given FieldBinder[Boolean] with
    def sqlType: Int = java.sql.Types.BOOLEAN
    def bind(stmt: PreparedStatement, idx: Int, v: Boolean): Int =
      stmt.setBoolean(idx, v)
      idx + 1
  given FieldBinder[Vector[Float]] with
    def sqlType: Int = java.sql.Types.ARRAY
    def bind(stmt: PreparedStatement, idx: Int, v: Vector[Float]): Int =
      val vecObj = new PGobject()
      vecObj.setType("ibm_extension.vector")
      vecObj.setValue(v.mkString("[", ",", "]"))
      stmt.setObject(idx, vecObj)
      idx + 1
  given FieldBinder[Float] with
    def sqlType: Int = java.sql.Types.FLOAT
    def bind(stmt: PreparedStatement, idx: Int, v: Float): Int =
      stmt.setFloat(idx, v)
      idx + 1
  given FieldBinder[java.time.LocalDate] with
    def sqlType: Int = java.sql.Types.DATE
    def bind(stmt: PreparedStatement, idx: Int, v: java.time.LocalDate): Int =
      val date = java.util.Date.from(
        v
          .atStartOfDay(ZoneId.systemDefault())
          .toInstant()
      );
      stmt.setDate(
        idx,
        new java.sql.Date(date.getTime())
      )
      idx + 1

  given FieldBinder[Long] with
    def sqlType: Int = java.sql.Types.BIGINT
    def bind(stmt: PreparedStatement, idx: Int, v: Long): Int =
      stmt.setLong(idx, v)
      idx + 1
  given FieldBinder[String] with
    def sqlType: Int = java.sql.Types.VARCHAR
    def bind(stmt: PreparedStatement, idx: Int, v: String): Int =
      stmt.setString(idx, v)
      idx + 1
  given FieldBinder[java.sql.Date] with
    def sqlType: Int = java.sql.Types.DATE
    def bind(stmt: PreparedStatement, idx: Int, v: java.sql.Date): Int =
      stmt.setDate(idx, v)
      idx + 1
  given FieldBinder[java.math.BigDecimal] with
    def sqlType: Int = java.sql.Types.DECIMAL
    def bind(stmt: PreparedStatement, idx: Int, v: java.math.BigDecimal): Int =
      stmt.setBigDecimal(idx, v)
      idx + 1
  given sbd: FieldBinder[scala.math.BigDecimal] with
    def sqlType: Int = java.sql.Types.DECIMAL
    def bind(stmt: PreparedStatement, idx: Int, v: scala.math.BigDecimal): Int =
      stmt.setBigDecimal(idx, v.bigDecimal)
      idx + 1

  given jsfb[T](using fg: FieldBinder[T]): FieldBinder[JsonString[T]] with
    def sqlType: Int = fg.sqlType

    def bind(stmt: PreparedStatement, idx: Int, v: JsonString[T]): Int =
      stmt.setString(idx, v.toString)
      idx + 1
  given FieldBinder[java.util.UUID] with
    def sqlType: Int = java.sql.Types.VARCHAR
    def bind(stmt: PreparedStatement, idx: Int, v: java.util.UUID): Int =
      stmt.setObject(idx, v, java.sql.Types.OTHER)
      idx + 1
  given FieldBinder[java.time.Instant] with
    def sqlType: Int = java.sql.Types.TIMESTAMP
    def bind(stmt: PreparedStatement, idx: Int, v: java.time.Instant): Int =
      stmt.setTimestamp(idx, java.sql.Timestamp.from(v))
      idx + 1
  given [T](using fb: FieldBinder[T]): FieldBinder[Default[T]] with
    def sqlType: Int = fb.sqlType
    def bind(stmt: PreparedStatement, idx: Int, opt: Default[T]): Int =
      fb.bind(stmt, idx, opt.value)
  given [T](using fb: FieldBinder[T]): FieldBinder[Option[T]] with
    def sqlType: Int = fb.sqlType
    def bind(stmt: PreparedStatement, idx: Int, opt: Option[T]): Int =
      opt match
        case Some(v) => fb.bind(stmt, idx, v)
        case None    => idx
        // stmt.setNull(idx, Types.VARCHAR)
        // idx + 1

  // Nullable marker trait
  given [T](using fb: FieldBinder[T]): FieldBinder[Nullable[T]] with
    def sqlType: Int = fb.sqlType
    def bind(stmt: PreparedStatement, idx: Int, v: Nullable[T]): Int =
      if v.asInstanceOf[AnyRef] == null then
        stmt.setNull(idx, Types.VARCHAR)
        idx + 1
      else fb.bind(stmt, idx, v.value)

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
    def toArray(v: Seq[UUID]): Array[AnyRef] =
      v.map(_.asInstanceOf[AnyRef]).toArray
