package org.pwharned.stream

sealed trait Stream[+A]:
  def map[B](f: A => B): Stream[B]
  def flatMap[B](f: A => Stream[B]): Stream[B]
  def filter(p: A => Boolean): Stream[A]
  def take(n: Int): Stream[A]
  def ++[B >: A](other: => Stream[B]): Stream[B]

  // Tail-recursive fold implementation
  final def fold[B](zero: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def loop(stream: Stream[A], acc: B): B = stream match {
      case Empty => acc
      case Cons(h, t) => loop(t(), f(acc, h()))
    }
    loop(this, zero)
  }

  // New methods for HTTP server functionality
  final def forEach[U](f: A => U): Unit = {
    @annotation.tailrec
    def loop(stream: Stream[A]): Unit = stream match {
      case Empty => ()
      case Cons(h, t) =>
        f(h())
        loop(t())
    }
    loop(this)
  }

  final def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  final def isEmpty: Boolean = this match {
    case Empty => true
    case _ => false
  }

  final def nonEmpty: Boolean = !isEmpty

  final def toList: List[A] = {
    @annotation.tailrec
    def loop(stream: Stream[A], acc: List[A]): List[A] = stream match {
      case Empty => acc.reverse
      case Cons(h, t) => loop(t(), h() :: acc)
    }
    loop(this, Nil)
  }

  final def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) =>
      val head = h()
      if p(head) then Cons(() => head, () => t().takeWhile(p))
      else Empty
  }

  final def drop(n: Int): Stream[A] = {
    @annotation.tailrec
    def loop(stream: Stream[A], remaining: Int): Stream[A] =
      if remaining <= 0 then stream
      else stream match {
        case Empty => Empty
        case Cons(_, t) => loop(t(), remaining - 1)
      }
    loop(this, n)
  }

  final def exists(p: A => Boolean): Boolean = {
    @annotation.tailrec
    def loop(stream: Stream[A]): Boolean = stream match {
      case Empty => false
      case Cons(h, t) => if p(h()) then true else loop(t())
    }
    loop(this)
  }

  final def forall(p: A => Boolean): Boolean = {
    @annotation.tailrec
    def loop(stream: Stream[A]): Boolean = stream match {
      case Empty => true
      case Cons(h, t) => if p(h()) then loop(t()) else false
    }
    loop(this)
  }

  // Safe evaluation - useful for potentially infinite streams
  final def safeForEach[U](f: A => U, maxElements: Int = 1000): Unit = {
    @annotation.tailrec
    def loop(stream: Stream[A], remaining: Int): Unit = {
      if remaining <= 0 then ()
      else stream match {
        case Empty => ()
        case Cons(h, t) =>
          f(h())
          loop(t(), remaining - 1)
      }
    }
    loop(this, maxElements)
  }

case object Empty extends Stream[Nothing]:
  def map[B](f: Nothing => B) = Empty
  def flatMap[B](f: Nothing => Stream[B]) = Empty
  def filter(p: Nothing => Boolean) = Empty
  def take(n: Int) = Empty
  def ++[B](other: => Stream[B]) = other

case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]:
  def map[B](f: A => B): Stream[B] =
    Cons(() => f(head()), () => tail().map(f))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    f(head()) ++ tail().flatMap(f)

  def filter(p: A => Boolean): Stream[A] =
    if p(head()) then Cons(head, () => tail().filter(p))
    else tail().filter(p)

  def take(n: Int): Stream[A] =
    if n <= 0 then Empty
    else Cons(head, () => tail().take(n - 1))

  def ++[B >: A](other: => Stream[B]): Stream[B] =
    Cons(head, () => tail() ++ other)

object Stream:
  def apply[A](elements: A*): Stream[A] =
    elements.foldRight(Empty: Stream[A])((a, acc) => Cons(() => a, () => acc))

  def unfold[A, S](seed: S)(f: S => Option[(A, S)]): Stream[A] =
    f(seed) match
      case None => Empty
      case Some((a, s)) => Cons(() => a, () => unfold(s)(f))

  // Utility constructors
  def from(start: Int): Stream[Int] =
    Cons(() => start, () => from(start + 1))

  def range(start: Int, end: Int): Stream[Int] =
    if start >= end then Empty
    else Cons(() => start, () => range(start + 1, end))

  def continually[A](value: A): Stream[A] =
    Cons(() => value, () => continually(value))

  def iterate[A](seed: A)(f: A => A): Stream[A] =
    Cons(() => seed, () => iterate(f(seed))(f))

  // Create a stream from an iterator or iterable
  def fromIterator[A](iterator: Iterator[A]): Stream[A] =
    if iterator.hasNext then
      Cons(() => iterator.next(), () => fromIterator(iterator))
    else Empty

  def fromIterable[A](iterable: Iterable[A]): Stream[A] =
    fromIterator(iterable.iterator)

implicit class StreamOps[A](private val s: Stream[A]) extends AnyVal {
  def toIterator: Iterator[A] = new Iterator[A] {
    private var cur: Stream[A] = s
    def hasNext: Boolean    = cur.nonEmpty
    def next(): A = cur match {
      case Cons(h, t) =>
        val a = h()
        cur = t()
        a
      case Empty => throw new NoSuchElementException("Stream empty")
    }
  }
}