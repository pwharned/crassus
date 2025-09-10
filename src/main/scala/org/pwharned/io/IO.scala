package org.pwharned.io

sealed trait IO[+A]:
  def map[B](f: A => B): IO[B]
  def flatMap[B](f: A => IO[B]): IO[B]
  def unsafeRun(): A

  def *>[B](next: => IO[B]): IO[B] =
    this.flatMap(_ => next)

  final def unsafeRunOptimized(): A = {
    // Simple depth limit to prevent stack overflow
    val maxDepth = 1000

    @annotation.tailrec
    def evaluateWithDepth(io: IO[A], depth: Int): A = {
      if (depth > maxDepth) {
        // Fall back to regular evaluation to prevent infinite recursion
        io.unsafeRun()
      } else {
        io match {
          case Pure(value) => value
          case Effect(thunk) => thunk()
          case FlatMapped(Pure(value), f) =>
            evaluateWithDepth(f(value), depth + 1)
          case FlatMapped(Effect(thunk), f) =>
            evaluateWithDepth(f(thunk()), depth + 1)
          case _ =>
            // For complex nested cases, use regular evaluation
            io.unsafeRun()
        }
      }
    }

    evaluateWithDepth(this, 0)
  }
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
