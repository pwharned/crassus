package org.pwharned.rpc

import org.pwharned.json.{JsonDeserializer, JsonSerializer}
import org.pwharned.parse.ParseError

import scala.reflect.ClassTag


// 1) One case‐class per parameter
case class RpcParam(
                     name:     String,
                     `type`:      String,  // or your own `TypeRepr`/`Schema` reference
                     required: Boolean = true,
                    properties: Option[List[RpcParam]] = None
                   )

// 2) One case‐class for the whole method
case class RpcMethodSchema(
                            method: String,
                            params: List[RpcParam],
                            result: String      // again, just the name of the return‐type
                          )


 
trait Schema[T] {

  def typeName: String
}

object Schema {
  /** summon a Schema if you have both a JsonDeserializer & JsonSerializer */
  def apply[T](using sch: Schema[T]): Schema[T] = sch

  /** automatically derive a Schema[T] from your existing parser+serializer */
  given fromJsonCodec[T](using
                         ct: ClassTag[T],
                         ds: JsonDeserializer[T],
                         js: JsonSerializer[T]   // assume you have the inverse typeclass
                        ): Schema[T] with
    def typeName: String = summon[scala.reflect.ClassTag[T]].runtimeClass.getSimpleName
}
