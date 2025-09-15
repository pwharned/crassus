package org.pwharned.sql.dialect

trait SqlDialect {
  def select(table: String, cols: Seq[String]): String
  def insertReturning(table: String, cols: Seq[String]): String
  def insertReturning[T<:Product](raw: String): String
  def updateReturning(raw: String): String
  def deleteReturning(raw: String): String
}
