package org.pwharned.database.dialect

trait SqlDialect {
  def select(table: String, cols: Seq[String]): String
  def insertReturning(table: String, cols: Seq[String]): String
  def insertReturning(raw: String): String

}
