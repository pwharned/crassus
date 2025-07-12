package org.pwharned.rpc

import org.pwharned.json.JsonSerializer

import scala.deriving.Mirror

trait RpcEndpoint[P<:Product, R<:Product](using js: JsonSerializer[R], mirror:Mirror.ProductOf[P]) {
  def schemaP: RpcSchema[P]
  def schemaR: RpcSchema[R]
  def call(p: P): R
  def name: String

  def decodeParams(args: List[Int|String]): Either[String,P]

  inline def invokeWith(args: List[Int|String]): Either[String, R] =
    decodeParams(args).map(call)
  def returnSerialized(p: P): String =
    js.serialize(call(p))
}