package org.pwharned.database

import org.pwharned.database.HKD.*
import org.pwharned.macros.typeName
import scala.quoted.*
import scala.compiletime.*
import scala.deriving.*
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
      val scalaType = inline unwrappedType[head] match
        case _: String => "String"
        case _: Option[String] => "Option[String]"
        case _: Int => "Int"
        case _: Option[Int] => "Option[Int]"
        case _: Boolean => "Boolean"
        case _: Option[Boolean] => "Option[Boolean]"
        case _: Double => "Double"
        case _: Option[Double] => "Option[Double]"
      mapper.mapType(scalaType) :: mapToSqlTypes[tail](mapper)

transparent inline def unwrappedTypeOld[T]: String =
  inline erasedValue[T] match {
    // If the type is an Option, preserve it by unwrapping its inner type:

    case _: String => "String"
    case _: Int => "Int"
    case _: Integer => "Int"
    case _: Boolean => "Boolean"
    case _: Double => "Double"
    case _: Float => "Float"
    case _: Long => "Long"
    case _: Short => "Short"
    case _: Byte => "Byte"
    case _: Option[t] => s"Option[${unwrappedType[t]}]"
    case _: PrimaryKey[t] => unwrappedType[t]
    case _: Nullable[t] => unwrappedType[t]
    // Recognize allowed base types:
    // If nothing matches, fail at compile time:
    case _ => error("Unsupported field type: " + typeName[T])
  }



transparent inline def summonFieldTypes[A <: Tuple]: List[String] =
    inline erasedValue[A] match {
      case _: EmptyTuple => Nil
      case _: (head *: tail) => unwrappedType[head] :: summonFieldTypes[tail]
    }


transparent inline def unwrappedType[T]: String = ${ unwrappedTypeImpl[T] }

def unwrappedTypeImpl[T: Type](using Quotes): Expr[String] = {
  import quotes.reflect.*

  def loop(tpe: TypeRepr): String = tpe.dealias match {
    // Base types: match directly against their dealiased representations.
    case t if t =:= TypeRepr.of[String]  => "String"
    case t if t =:= TypeRepr.of[Int]     => "Int"
    case t if t =:= TypeRepr.of[Integer] => "Int"
    case t if t =:= TypeRepr.of[Boolean] => "Boolean"
    case t if t =:= TypeRepr.of[Double]  => "Double"
    case t if t =:= TypeRepr.of[Float]   => "Float"
    case t if t =:= TypeRepr.of[Long]    => "Long"
    case t if t =:= TypeRepr.of[Short]   => "Short"
    case t if t =:= TypeRepr.of[Byte]    => "Byte"

    // Handle Option by unwrapping its inner type.
    case AppliedType(t, List(arg)) if t.typeSymbol.name == "Option" =>
      s"Option[${loop(arg)}]"

    // For opaque types (or wrappers), check the type symbol and unwrap.
    case AppliedType(t, List(arg))
      if t.typeSymbol.name == "Id" ||
        t.typeSymbol.name == "PrimaryKey" ||
        t.typeSymbol.name == "Nullable" =>
      loop(arg)

    case other =>
      report.error(s"Unsupported field type: ${other.show}")
      "Unsupported"
  }

  Expr(loop(TypeRepr.of[T]))
}
