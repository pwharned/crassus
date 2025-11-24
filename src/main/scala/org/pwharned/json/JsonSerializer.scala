package org.pwharned.json

trait JsonSerializer[T]:
  def serialize(obj:T): String
