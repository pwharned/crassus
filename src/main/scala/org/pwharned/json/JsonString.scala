package org.pwharned.json

import org.pwharned.database.statements.Rs
import org.pwharned.openapi.{Schema, schema}
import org.pwharned.parse.{Parser, Primitives}
import org.pwharned.parse.QueryDeserializer.QueryFieldDeserializer

opaque type JsonString = String

object JsonString:
  def apply(s: String): JsonString = s

  extension (j: JsonString)
    def value: String = j
  given JsonSerializer[JsonString] with
    def toJson(js: JsonString): String = js.value

    override def serialize(obj: JsonString): JsonString = obj.value
  given Rs[JsonString] with
    def read(r: java.sql.ResultSet, c: String): JsonString = r.getString(c)
  given QueryFieldDeserializer[JsonString] with 
    override def parser: Parser[JsonString] = Primitives.stringNoAmpersand
  given Schema[JsonString] with
    def labels: Nil.type = Nil
    def `type`: Option[JsonString] = Some("string")

    def toSchema: schema = schema(`type` = `type`)


// 👇 Implicit conversion from String to JsonString
given Conversion[String, JsonString] with
  def apply(s: String): JsonString = JsonString(s)
