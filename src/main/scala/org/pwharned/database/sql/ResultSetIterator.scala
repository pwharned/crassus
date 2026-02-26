package org.pwharned.database.sql

final class ResultSetIterator[A](
    rs: java.sql.ResultSet,
    stmt: java.sql.Statement,
    row: Row[A]
) extends Iterator[A]:

  private var hasNextRow: Boolean = rs.next()
  private var closed: Boolean = false

  private def closeResources(): Unit =
    if !closed then
      try rs.close()
      catch case _ => ()
      try stmt.close()
      catch case _ => ()
      closed = true

  override def hasNext: Boolean =
    if !hasNextRow then closeResources()
    hasNextRow

  override def next(): A =
    if !hasNext then throw new NoSuchElementException("No more rows")
    val value = row.fromRs(rs)
    hasNextRow = rs.next()
    if !hasNextRow then closeResources()
    value
