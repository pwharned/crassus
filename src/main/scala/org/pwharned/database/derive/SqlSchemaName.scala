package org.pwharned.database.derive

import scala.compiletime.*
import scala.concurrent.{ExecutionContext, Future}
import scala.deriving.*
import scala.language.implicitConversions

trait SqlSchemaName[T] {

  def schema(): Option[String]

}
object SqlSchemaName:

  inline def derived[T <: Product](using
      m: Mirror.ProductOf[T]
  ): SqlSchemaName[T] =
    val fullName: String = constValue[m.MirroredLabel]
    val schemaAndTable = fullName.split("\\.")
    val schemaName = schemaAndTable.length match
      case 1 => None
      case 2 => Some(schemaAndTable(0))
    () => schemaName

  inline given auto[T <: Product](using
      m: Mirror.ProductOf[T]
  ): SqlSchemaName[T] =
    derived[T]
extension [T <: Product](entity: T)(using sql: SqlSchemaName[T])
  def schemaName: Option[String] = summon[SqlSchemaName[T]].schema()
