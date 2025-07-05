package org.pwharned.`lazy`

final class Lazy[T](private val thunk: () => T):
  lazy val value: T = thunk()

object Lazy:
  inline given [T](using inline t: => T): Lazy[T] =
    new Lazy(() => t)

