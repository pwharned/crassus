// src/main/scala/org/pwharned/json/JsonSerializer.scala
package org.pwharned.json

import scala.language.implicitConversions
import scala.compiletime.*
import scala.deriving.Mirror
import org.pwharned.`lazy`.Lazy
import org.pwharned.`lazy`.Lazy.given

// A type‐class that knows how to “unwrap” a wrapper type F[A] to A at runtime
trait JsWrap[F[_], A]:
  def wrap(fa: F[A], serializeA: A => String): String

object JsWrap:
  /** If you have a Conversion[F[A],A], just unwrap the single‐field wrapper. */
  given singleFieldWrap[F[_], A](using conv: Conversion[F[A], A]): JsWrap[F, A] with
    def wrap(fa: F[A], serializeA: A => String): String =
      val a = conv(fa)
      if a == null then "null" else serializeA(a)

// The JSON serializer type‐class
trait JsonSerializer[T]:
  def serialize(obj: T): String

object JsonSerializer:

  // === 1) Derivation for case‐classes / products ===
  inline given  derivedProduct[T <: Product](using
                                                   m: Mirror.ProductOf[T]
                                                  ): JsonSerializer[T] =
    // field names
    val labels  = constValueTuple[m.MirroredElemLabels]
      .toList
      .map(_.toString)

    // summon each field‐serializer *lazily* into a homogeneous List
    val lazySer = summonAllLazy[m.MirroredElemTypes]

    // build the runtime serializer
    makeProductSerializer(labels, lazySer)

  // Summon a List of Lazy[JsonSerializer[Any]] from Tuple types
  private inline def summonAllLazy[Elems <: Tuple]: List[Lazy[JsonSerializer[Any]]] =
    inline erasedValue[Elems] match
      case _: EmptyTuple => Nil
      case _: (h *: t)   =>
        // cast each Lazy[JsonSerializer[h]] → Lazy[JsonSerializer[Any]]
        summonInline[Lazy[JsonSerializer[h]]]
          .asInstanceOf[Lazy[JsonSerializer[Any]]] ::
          summonAllLazy[t]

  // The actual “tiny” serializer that only loops at runtime
  private def makeProductSerializer[T](
                                        labels: List[String],
                                        lazySer: List[Lazy[JsonSerializer[Any]]]
                                      ): JsonSerializer[T] = new JsonSerializer[T]:
    def serialize(obj: T): String =
      val values = obj.asInstanceOf[Product].productIterator.toList
      val fields = labels
        .lazyZip(lazySer)
        .lazyZip(values)
        .collect { case (name, lser, v)
          if v != null && v != None && v != Nil =>
          // now force the Lazy and cast back to Any
          val ser = lser.value.asInstanceOf[JsonSerializer[Any]]
          s""""$name":${ser.serialize(v)}"""
        }
      s"{${fields.mkString(",")}}"

  // === 2) Base cases ===
  given JsonSerializer[String] with
    def serialize(s: String): String =
      if s == null then "null" else "\"" + s.replace("\"", "\\\"") + "\""

  given JsonSerializer[Boolean] with 
    def serialize(b: Boolean): String = b.toString
  given JsonSerializer[Int]     with 
    def serialize(i: Int): String     = i.toString
  given JsonSerializer[Long]    with 
    def serialize(l: Long): String    = l.toString
  given JsonSerializer[Float]   with 
    def serialize(f: Float): String   = f.toString

  given JsonSerializer[java.util.UUID] with
    def serialize(u: java.util.UUID): String = "\"" + u.toString + "\""

  // === 3) Collections and maps ===
  given mapSerializer[A](using base: JsonSerializer[A]): JsonSerializer[Map[String, A]] with
    def serialize(m: Map[String, A]): String =
      m.map { (k, v) => "\"" + k + "\":" + base.serialize(v) }
        .mkString("{", ",", "}")

  given listSerializer[A](using base: JsonSerializer[A]): JsonSerializer[List[A]] with
    def serialize(xs: List[A]): String = xs.map(base.serialize).mkString("[", ",", "]")

  given iteratorSerializer[A](using base: JsonSerializer[A]): JsonSerializer[Iterator[A]] with
    def serialize(it: Iterator[A]): String = it.map(base.serialize).mkString("[", ",", "]")

  given optionSerializer[A](using base: JsonSerializer[A]): JsonSerializer[Option[A]] with
    def serialize(opt: Option[A]): String = opt match
      case Some(v) => base.serialize(v)
      case None    => "null"

  // === 4) Unions and higher‐kinds ===
  given unionSerializer[A, B](using
                              sa: JsonSerializer[A],
                              sb: JsonSerializer[B]
                             ): JsonSerializer[A | B] with
    def serialize(ab: A | B): String = ab match
      case a: A => sa.serialize(a)
      case b: B => sb.serialize(b)

  given hktSerializer[F[_], A](using
                               base:    JsonSerializer[A],
                               wrapper: JsWrap[F, A]
                              ): JsonSerializer[F[A]] with
    def serialize(fa: F[A]): String = wrapper.wrap(fa, base.serialize)

end JsonSerializer

// Extension method for direct `.serialize` calls
extension [T](value: T)(using ser: JsonSerializer[T])
  def serialize: String = ser.serialize(value)
