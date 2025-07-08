package org.pwharned.database.statements

trait SqlDialect {
  def select(table: String, cols: Seq[String]): String
  def insertReturning(table: String, cols: Seq[String]): String
  def insertReturning(raw: String): String

}
