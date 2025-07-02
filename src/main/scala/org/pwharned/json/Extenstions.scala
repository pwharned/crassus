package org.pwharned.json

import org.pwharned.parse.ParseError

extension (s: String)
  def deserialize[A <: Product](using j: JsonDeserializer[A]): Either[ParseError, (A, String)] = summon[JsonDeserializer[A]].deserialize.apply(s)

