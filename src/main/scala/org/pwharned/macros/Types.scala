package org.pwharned.macros

import scala.deriving.*
import scala.compiletime.*
import generated.PrimaryKey
trait DbTypeMapper:
  def mapType(scalaType: String): String

object PostgresTypeMapper extends DbTypeMapper:
  def mapType(scalaType: String): String = scalaType match
    case "String" => "TEXT"
    case "Int" => "INTEGER"
    case "Option[Int]" => "INTEGER NULL"
    case "Boolean" => "BOOLEAN"
    case "Double" => "DOUBLE PRECISION"
    case "Option[Double]" => "DOUBLE PRECISION NULL"
    case _ => "TEXT"

object Db2TypeMapper extends DbTypeMapper:
  def mapType(scalaType: String): String = scalaType match
    case "String" => "VARCHAR(255)"
    case "Int" => "INTEGER"
    case "Option[Int]" => "INTEGER NULL"
    case "Boolean" => "SMALLINT" // DB2 uses SMALLINT for boolean
    case "Double" => "DECIMAL(15,2)"
    case "Option[Double]" => "DECIMAL(15,2) NULL"
    case _ => "VARCHAR(255)"

transparent inline def mapToSqlTypes[A <: Tuple](mapper: DbTypeMapper): List[String] =
  inline erasedValue[A] match
    case _: EmptyTuple => Nil
    case _: (head *: tail) =>
      val scalaType = inline erasedValue[head] match
        case _: String => "String"
        case _: Option[String] => "Option[String]"
        case _: Int => "Int"
        case _: Option[Int] => "Option[Int]"
        case _: Boolean => "Boolean"
        case _: Option[Boolean] => "Option[Boolean]"
        case _: Double => "Double"
        case _: Option[Double] => "Option[Double]"
        case _ => "Hello"
      mapper.mapType(scalaType) :: mapToSqlTypes[tail](mapper)

transparent inline def summonFieldTypes[A <: Tuple]: List[String] = {

  inline erasedValue[A] match {
    case _: EmptyTuple => Nil
    case _: (head *: tail) =>
      val headType = inline erasedValue[head] match
        case _: PrimaryKey[Int] => "Int"
        case _: String => "String"
        case _: Option[String] => "Option[String]"
        case _: Int => "Int"
        case _: Integer => "Int"
        case _: Boolean => "Boolean"
        case _: Double => "Double"
        case _: Float => "Float"
        case _: Long => "Long"
        case _: Short => "Short"
        case _: Byte => "Byte"
        case _:PrimaryKey[t] => "PrimaryKey"
        case _ => {
          "Unknown"
        }
      headType :: summonFieldTypes[tail]
  }
}