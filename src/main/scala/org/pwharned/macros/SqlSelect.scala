package org.pwharned.macros

import scala.deriving.*
import scala.compiletime.*

trait Sql[T] {
  def names: List[String]
  def name: String
  def select: String
  def getClassesFieldType: List[String]
  def fromResultSet(rs: java.sql.ResultSet): T

}

transparent inline def summonFieldTypes[A <: Tuple]: List[String] = {

  inline erasedValue[A] match {
    case _: EmptyTuple => Nil
    case _: (head *: tail) =>
      val headType = inline erasedValue[head] match
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
        case _ => "Unknown"
      headType :: summonFieldTypes[tail]
  }
}

object Sql:
  transparent inline given derived[T <: Product](using m: Mirror.ProductOf[T]): Sql[T] = {
    new Sql[T] {
      def name: String = constValue[m.MirroredLabel]
      def names: List[String] =
        constValueTuple[m.MirroredElemLabels].toIArray.toList.map(_.toString)
      def select: String = s"select ${names.mkString(",") } from ${name}"

      def fromResultSet(rs: java.sql.ResultSet):T = {
        val labels = constValueTuple[m.MirroredElemLabels].toIArray.toList.map(_.toString)
        val zipped = labels.zip(getClassesFieldType)
        val extractedValues = zipped.map {
          case (label, "String") => rs.getString(label)
          case (label, "Option[String]") => Option(rs.getString(label))
          case (label, "Int") => rs.getInt(label)
          case (label, "Boolean") => rs.getBoolean(label)
          case (label, "Double") => rs.getDouble(label)
          case (label, "Float") => rs.getFloat(label)
          case (label, "Long") => rs.getLong(label)
          case (label, "Short") => rs.getShort(label)
          case (label, "Byte") => rs.getByte(label)
          case _ => throw new IllegalArgumentException(s"Unsupported field type")
        }

        // Convert to Tuple for Mirror's apply method
        val valuesTuple = Tuple.fromArray(extractedValues.toArray)

        // Use Mirror to instantiate case class
        m.fromProduct(valuesTuple)
      }

      def getClassesFieldType: List[String] = {
        inline m match {
          case m: Mirror.ProductOf[T] => {

            summonFieldTypes[m.MirroredElemTypes]
          }

        }
      }

    }
  }

extension [T<:Product](entity: T)(using sql: Sql[T])
  def fields: List[String] = summon[Sql[T]].names
  def select: String = summon[Sql[T]].select
  def classFieldTypes: List[String] = summon[Sql[T]].getClassesFieldType

extension (rs: java.sql.ResultSet)
  inline def as[A <: Product](using sql: Sql[A]): A =
    sql.fromResultSet(rs)


extension (con: java.sql.Connection)
  inline def query[A <: Product](using sql: Sql[A]): Iterator[A] =
    val stmt = con.prepareStatement(sql.select)
    val rs = stmt.executeQuery()
    Iterator.continually(rs.next()).takeWhile(identity).map( x => rs.as[A])




extension (con: java.sql.Connection)
  inline def streamQuery[A <: Product](batchSize: Int)(using sql: Sql[A]): java.sql.Connection => Iterator[Seq[A]] = con =>
    val stmt = con.prepareStatement(sql.select)
    val rs = stmt.executeQuery()

    Iterator.continually(rs.next())
      .takeWhile(identity)
      .map( x => rs.as[A]).grouped(batchSize)



