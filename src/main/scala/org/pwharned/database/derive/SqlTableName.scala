package org.pwharned.database.derive

import scala.compiletime.*
import scala.concurrent.{ExecutionContext, Future}
import scala.deriving.*
import scala.language.implicitConversions

trait SqlTableName[T] {

  def name(): String

}
object SqlTableName:

  inline def derived[T <: Product](using
      m: Mirror.ProductOf[T]
  ): SqlTableName[T] =
    val fullName: String = constValue[m.MirroredLabel]
    val schemaAndTable = fullName.split(".")
    val tableName = schemaAndTable.length match
      case 1 => schemaAndTable(0)
      case 2 => schemaAndTable(1)
    () => tableName

  inline given auto[T <: Product](using
      m: Mirror.ProductOf[T]
  ): SqlTableName[T] =
    derived[T]
extension [T <: Product](entity: T)(using sql: SqlTableName[T])
  def tableName: String = summon[SqlTableName[T]].name()
