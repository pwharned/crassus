package org.pwharned.database.statements

object PostgresDialect extends SqlDialect:
  def select(table: String, cols: Seq[String]) =
    s"SELECT ${cols.mkString(",")} FROM $table;"

  def insertReturning(table: String, cols: Seq[String]): String =
    val cs = cols.mkString(", ")
    val ps = List.fill(cols.size)("?").mkString(", ")
    s"INSERT INTO $table ($cs) VALUES ($ps) RETURNING *;"

  def insertNoReturn(table: String, cols: Seq[String]): String =
    val cs = cols.mkString(", ")
    val ps = List.fill(cols.size)("?").mkString(", ")
    s"INSERT INTO $table ($cs) VALUES ($ps);"


