package org.pwharned.database.sql

object Db2Dialect extends SqlDialect:
  def select(table: String, cols: Seq[String]) =
    s"SELECT ${cols.mkString(",")} FROM $table"

  def insertReturning(table: String, cols: Seq[String]): String =
    val cs = cols.mkString(", ")
    val ps = List.fill(cols.size)("?").mkString(", ")
    s"SELECT $cs FROM FINAL TABLE (INSERT INTO $table ($cs) VALUES ($ps))"

  def updateReturning(raw: String): String =
    s"SELECT * FROM FINAL TABLE ($raw)"
  def deleteReturning(raw: String): String =
    s"SELECT * FROM OLD TABLE($raw)"
  def insertNoReturn(table: String, cols: Seq[String]): String =
    val cs = cols.mkString(", ")
    val ps = List.fill(cols.size)("?").mkString(", ")
    s"INSERT INTO $table ($cs) VALUES ($ps)"
  inline def updateReturning[T](rawUpdate: String): String =
    val (tableName, isColumnOrganized) = TableOrganizationMacro.tableInfo[T]
    if isColumnOrganized then rawUpdate // Just return the raw update
    else s"SELECT * FROM FINAL TABLE ($rawUpdate)"

  inline def limitAndOffset(
      raw: String,
      limit: Option[Int],
      offset: Option[Int]
  ): String =
    (limit, offset) match
      case (Some(l), Some(o)) =>
        s"select * from ($raw) as t offset $o rows fetch next $l rows only"
      case (Some(l), None) =>
        s"select * from ($raw) as t fetch next $l rows only"
      case (None, Some(o)) =>
        s"select * from ($raw) as t offset $o rows"
      case _ =>
        raw

  inline def insertReturning[T](raw: String): String =
    val (tableName, isColumnOrganized) = TableOrganizationMacro.tableInfo[T]
    if isColumnOrganized then
      // Column-organized table: use regular insert without returning
      raw
    else s"SELECT * FROM FINAL TABLE ($raw)"
