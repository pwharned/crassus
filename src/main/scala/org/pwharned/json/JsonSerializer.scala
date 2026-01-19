package org.pwharned.json

import org.pwharned.json

import scala.language.implicitConversions
import scala.compiletime.*
import scala.deriving.*
import org.pwharned.database.hkd._

trait LowPriorityJsonSerializer:
  given fallbackSerializer[T]: JsonSerializer[T] with
    def serialize(ob: T): String = "\"" + ob.toString + "\""
trait JsWrap[F[_], A]:
  def wrap(fa: F[A], serializeA: A => String): String

object JsWrap:
  /** Universal “unwrap the single‐field wrapper F[A] into the A” instance. No
    * Mirror required here; we just need a Conversion[F[A],A].
    */
  inline given singleFieldWrap[F[_], A](using
      conv: Conversion[F[A], A]
  ): JsWrap[F, A] with
    def wrap(value: F[A], ser: A => String): String =
      val a = conv(value)
      if a == null then "null" // raw null → JSON null
      else ser(a)

trait JsonSerializer[T]:
  def serialize(obj: T): String

object JsonSerializer extends LowPriorityJsonSerializer:
  def escapeJsonString(s: String): String =
    s.flatMap {
      case '"'          => "\\\""
      case '\\'         => "\\\\"
      case '\n'         => "\\n"
      case '\r'         => "\\r"
      case '\t'         => "\\t"
      case '\b'         => "\\b"
      case '\f'         => "\\f"
      case c if c < ' ' => f"\\u${c.toInt}%04x"
      case c            => c.toString
    }
  given fieldBasedSerializer[T, R](using
      fr: FieldReducer.Aux[T, R],
      js: JsonSerializer[R]
  ): JsonSerializer[T] with
    def serialize(obj: T): String =
      js.serialize(fr.unwrap(obj))
  inline given derived[T <: Product](using
      m: Mirror.ProductOf[T]
  ): JsonSerializer[T] =
    new JsonSerializer[T]:
      def serialize(obj: T): String =
        val fieldNames =
          constValueTuple[m.MirroredElemLabels].toIArray.toList.map(_.toString)
        val productValues = obj.asInstanceOf[Product].productIterator.toList
        val readers = summonAllUnwrapped[m.MirroredElemTypes]
          .map(_.asInstanceOf[JsonSerializer[Any]]) // widen so we can .read
        // 3. read each column (you could prefix the column

        val fields = fieldNames
          .zip(readers.zip(productValues))
          .filter { case (name, (ser, v)) =>
            v != null & v != None & v != Nil
          }
          .map { case (name, (ser, v)) =>
            s""""$name":${ser.serialize(v)}"""
          }

        s"{${fields.mkString(",")}}"

      def serialize(obj: List[T]): String =
        "[" + obj.map(x => serialize(x)).mkString(",") + "]"

      def serialize(obj: Iterator[T]): String = obj
        .foldLeft("[")((acc, x) => acc + serialize(x) + ",")
        .stripSuffix(",") + "]"

  given JsonSerializer[String] with
    def serialize(ob: String): String = {
      if ob == null then "null" else s"\"${escapeJsonString(ob)}\""
    }
  /*
    given unionSerializer[A, B](using
                                sa: JsonSerializer[A],
                                sb: JsonSerializer[B]
                               ): JsonSerializer[A | B] with

      def serialize(ab: A | B): String = ab match
        case a: A => sa.serialize(a)
        case b: B => sb.serialize(b)
   */
  given JsonSerializer[Boolean] with
    def serialize(ob: Boolean): String = ob.toString
  given JsonSerializer[Float] with
    def serialize(ob: Float): String = ob.toString
  given JsonSerializer[Long] with
    def serialize(ob: Long): String = ob.toString
  given JsonSerializer[Int] with
    def serialize(ob: Int): String = ob.toString
  given JsonSerializer[java.util.UUID] with
    def serialize(ob: java.util.UUID): String = s"\"${ob.toString}\""
  given JsonSerializer[java.time.Instant] with
    def serialize(ob: java.time.Instant): String = s"\"${ob.toString}\""
  given JsonSerializer[scala.math.BigDecimal] with
    def serialize(ob: scala.math.BigDecimal): String = { ob.toString }

  given mapSerializer[A](using
      base: JsonSerializer[A]
  ): JsonSerializer[Map[String, A]] with
    def serialize(ob: Map[String, A]): String = "{" + ob
      .map(x => {
        s"\"${x._1}\":${base.serialize(x._2)}"
      })
      .mkString(",") + "}"
  given vecSerializer[A](using
      base: JsonSerializer[A]
  ): JsonSerializer[Vector[A]] with
    def serialize(ob: Vector[A]): String =
      "[" + ob.map(x => base.serialize(x)).mkString(",") + "]"
  given listSerializer[A](using
      base: JsonSerializer[A]
  ): JsonSerializer[List[A]] with
    def serialize(ob: List[A]): String =
      "[" + ob.map(x => base.serialize(x)).mkString(",") + "]"
  given iteratorSerializer[A](using
      base: JsonSerializer[A]
  ): JsonSerializer[Iterator[A]] with
    def serialize(ob: Iterator[A]): String =
      "[" + ob.map(x => base.serialize(x)).mkString(",") + "]"
  given optionSerializer[A](using
      base: JsonSerializer[A]
  ): JsonSerializer[Option[A]] with
    def serialize(ob: Option[A]): String = ob match {
      case Some(value) => summonInline[JsonSerializer[A]].serialize(value)
      case None        => "null"
    }

  private inline def summonAllUnwrapped[Elems <: Tuple]
      : List[JsonSerializer[?]] =
    inline erasedValue[Elems] match
      case _: EmptyTuple => Nil
      case _: (h *: t) =>
        summonInline[JsonSerializer[h]] :: summonAllUnwrapped[t]

extension [T](obj: T)(using json: JsonSerializer[T])
  inline def serialize: String = summon[JsonSerializer[T]].serialize(obj)
extension [T <: Product](obj: Iterator[T])(using json: JsonSerializer[T])
  inline def serialize: String =
    summon[JsonSerializer[Iterator[T]]].serialize(obj)
