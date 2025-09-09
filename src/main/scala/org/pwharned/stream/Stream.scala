package org.pwharned.stream

sealed trait Stream[+A]:
  def map[B](f: A => B): Stream[B]
  def flatMap[B](f: A => Stream[B]): Stream[B]
  def filter(p: A => Boolean): Stream[A]
  def take(n: Int): Stream[A]
  def fold[B](zero: B)(f: (B, A) => B): B
  def ++[B >: A](other: => Stream[B]): Stream[B]

case object Empty extends Stream[Nothing]:
  def map[B](f: Nothing => B) = Empty
  def flatMap[B](f: Nothing => Stream[B]) = Empty
  def filter(p: Nothing => Boolean) = Empty
  def take(n: Int) = Empty
  def fold[B](zero: B)(f: (B, Nothing) => B) = zero
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

  def fold[B](zero: B)(f: (B, A) => B): B =
    tail().fold(f(zero, head()))(f)

  def ++[B >: A](other: => Stream[B]): Stream[B] =
    Cons(head, () => tail() ++ other)

object Stream:
  def apply[A](elements: A*): Stream[A] =
    elements.foldRight(Empty: Stream[A])((a, acc) => Cons(() => a, () => acc))

  def unfold[A, S](seed: S)(f: S => Option[(A, S)]): Stream[A] =
    f(seed) match
      case None => Empty
      case Some((a, s)) => Cons(() => a, () => unfold(s)(f))
