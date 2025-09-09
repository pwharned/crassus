package org.pwharned.codec

import scala.collection.AbstractIterable
import scala.compiletime.{erasedValue, error, summonInline}
import scala.deriving.*

inline def summonInstances[T, Elems <: Tuple](self: => Eq[T]): List[Eq[?]] =
  inline erasedValue[Elems] match
    case _: (elem *: elems) => deriveOrSummon[T, elem](self) :: summonInstances[T, elems](self)
    case _: EmptyTuple      => Nil

inline def deriveOrSummon[T, Elem](self: => Eq[T]): Eq[Elem] =
  inline erasedValue[Elem] match
    case _: T => self.asInstanceOf[Eq[Elem]]
    case _    => summonInline[Eq[Elem]]

trait Eq[T]:
  def eqv(x: T, y: T): Boolean

object Eq:
  given Eq[Int] with
    def eqv(x: Int, y: Int) = x == y
  given Eq[String] with
    def eqv(x: String, y: String) = x == y

  // By-name element instance to allow recursive tying of knots
  given [A](using eqA: => Eq[A]): Eq[List[A]] with
    def eqv(x: List[A], y: List[A]): Boolean =
      (x.length == y.length) && x.lazyZip(y).forall(eqA.eqv)

  def check(x: Any, y: Any, elem: Eq[?]): Boolean =
    elem.asInstanceOf[Eq[Any]].eqv(x, y)

  def iterable[T](p: T): Iterable[Any] = new AbstractIterable[Any]:
    def iterator: Iterator[Any] = p.asInstanceOf[Product].productIterator

  def eqSum[T](s: Mirror.SumOf[T], elems: => List[Eq[?]]): Eq[T] =
    new Eq[T]:
      def eqv(x: T, y: T): Boolean =
        val ox = s.ordinal(x)
        (s.ordinal(y) == ox) && check(x, y, elems(ox))

  def eqProduct[T](p: Mirror.ProductOf[T], elems: => List[Eq[?]]): Eq[T] =
    new Eq[T]:
      def eqv(x: T, y: T): Boolean =
        iterable(x).lazyZip(iterable(y)).lazyZip(elems).forall(check)

  inline given derived[T](using m: Mirror.Of[T]): Eq[T] =
    // Tie the recursive knot
    lazy val self: Eq[T] =
      inline m match
        case s: Mirror.SumOf[T]     => eqSum(s, elemInstances)
        case p: Mirror.ProductOf[T] => eqProduct(p, elemInstances)
    lazy val elemInstances = summonInstances[T, m.MirroredElemTypes](self)
    self
end Eq

@main def test(): Unit =
  case class Hello(head: String, next: List[Hello] = Nil) // no derives needed; summon uses Eq.derived
  println(summon[Eq[Hello]].eqv(
    Hello("a", List(Hello("c"))),
    Hello("a", List(Hello("c")))
  ))
