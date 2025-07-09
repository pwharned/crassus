package org.pwharned.database.dialect

import org.pwharned.database.dialect.SqlDialect

object Db2Dialect extends SqlDialect:
  def select(table: String, cols: Seq[String]) =
    s"SELECT ${cols.mkString(",")} FROM $table;"

  def insertReturning(table: String, cols: Seq[String]): String =
    val cs = cols.mkString(", ")
    val ps = List.fill(cols.size)("?").mkString(", ")
    s"SELECT $cs FROM FINAL TABLE (INSERT INTO $table ($cs) VALUES ($ps))"
  
  def insertReturning(raw: String): String =
    s"SELECT * FROM FINAL TABLE ($raw)"

  def insertNoReturn(table: String, cols: Seq[String]): String =
    val cs = cols.mkString(", ")
    val ps = List.fill(cols.size)("?").mkString(", ")
    s"INSERT INTO $table ($cs) VALUES ($ps);"
