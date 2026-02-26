package org.pwharned.database.sql

object PostgresDialect extends SqlDialect:
  def select(table: String, cols: Seq[String]) =
    s"SELECT ${cols.mkString(",")} FROM $table"

  def insertReturning(table: String, cols: Seq[String]): String =
    val cs = cols.mkString(", ")
    val ps = List.fill(cols.size)("?").mkString(", ")
    s"INSERT INTO $table ($cs) VALUES ($ps) RETURNING *"

  def insertReturning[T](raw: String): String =
    s"$raw RETURNING *"
  def deleteReturning(raw: String): String = s"$raw RETURNING *"
  override def updateReturning(raw: String): String = s"$raw RETURNING *"
  def insertNoReturn(table: String, cols: Seq[String]): String =
    val cs = cols.mkString(", ")
    val ps = List.fill(cols.size)("?").mkString(", ")
    s"INSERT INTO $table ($cs) VALUES ($ps)"
  inline def limitAndOffset(
      raw: String,
      limit: Option[Int],
      offset: Option[Int]
  ): String =
    (limit, offset) match
      case (Some(l), Some(o)) =>
        s"$raw limit $l offset $o"
      case (Some(l), None) =>
        s"$raw limit $l"
      case (None, Some(o)) =>
        s"$raw offset $o"
      case _ =>
        raw
