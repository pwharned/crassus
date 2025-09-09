package org.pwharned.io

sealed trait IO[+A]:
  def map[B](f: A => B): IO[B]
  def flatMap[B](f: A => IO[B]): IO[B]
  def unsafeRun(): A

case class Pure[A](value: A) extends IO[A]:
  def map[B](f: A => B) = Pure(f(value))
  def flatMap[B](f: A => IO[B]) = f(value)
  def unsafeRun() = value

case class Effect[A](thunk: () => A) extends IO[A]:
  def map[B](f: A => B) = Effect(() => f(thunk()))
  def flatMap[B](f: A => IO[B]) = Effect(() => f(thunk()).unsafeRun())
  def unsafeRun() = thunk()

case class FlatMapped[A, B](source: IO[A], f: A => IO[B]) extends IO[B]:
  def map[C](g: B => C) = FlatMapped(source, (a: A) => f(a).map(g))
  def flatMap[C](g: B => IO[C]) = FlatMapped(source, (a: A) => f(a).flatMap(g))
  def unsafeRun() = f(source.unsafeRun()).unsafeRun()

object IO:
  def pure[A](a: A): IO[A] = Pure(a)
  def effect[A](a: => A): IO[A] = Effect(() => a)
  def println(s: String): IO[Unit] = Effect(() => scala.Predef.println(s))
