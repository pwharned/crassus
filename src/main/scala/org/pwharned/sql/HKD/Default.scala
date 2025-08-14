package org.pwharned.sql.HKD

sealed trait Default[X]:
  def value: X

object Default:
  def apply[X](x: X): Default[X] = x

  given [T]: Conversion[T, Default[T]] = x => Default(x)

  given [T]: Conversion[Default[T], T] = x => x.value
