package org.pwharned.json

import org.pwharned.database.sql.Rs
import org.pwharned.openapi.{Schema, schema}
import org.pwharned.parse.{Parser, Primitives}
import org.pwharned.parse.QueryDeserializer.QueryFieldDeserializer
import org.pwharned.parse.Parse.*

import scala.deriving.Mirror
opaque type JsonString[T] = String

object JsonString:
  def apply[T](s: String): JsonString[T] = s

  extension [T](j: JsonString[T]) // Added [T] parameter
    def value: String = j

  given [T]: JsonSerializer[JsonString[T]] with // Added [T] parameter
    def toJson(js: JsonString[T]): String = js.value

    // Fixed return type - should return String, not JsonString[T]
    override def serialize(obj: JsonString[T]): String = obj.value

  given [T]: Rs[JsonString[T]] with // Added [T] parameter
    def read(r: java.sql.ResultSet, c: String): JsonString[T] =
      JsonString[T](r.getString(c)) // Properly construct JsonString[T]

  given [T]: QueryFieldDeserializer[JsonString[T]] with // Added [T] parameter
    override def parser: Parser[JsonString[T]] =
      Primitives.stringNoAmpersand.map(JsonString[T]) // Map to JsonString[T]

  given [T]: Conversion[String, JsonString[T]] with
    def apply(s: String): JsonString[T] = JsonString[T](s)
