package org.pwharned.database.hkd

sealed trait  Nullable[X]:
  def value: X

object Nullable:
  def apply[X](x: X): Nullable[X] = Nullable(x)

  given [T]: Conversion[T, Nullable[T]] = x => Nullable(x)
  given [T]: Conversion[Nullable[T], T] = x => x.value