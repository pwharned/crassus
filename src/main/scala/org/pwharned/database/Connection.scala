package org.pwharned.database

import org.pwharned.database.Row
import org.pwharned.database.macros.Select
import scala.concurrent.{ExecutionContext, Future}

object Connection:
  extension (con: java.sql.Connection)
    def query[A](using sql: Select[A], row: Row[A]): Iterator[A] =
      val stmt = con.prepareStatement(sql.select)

      val rs = stmt.executeQuery()
      Iterator.continually(rs.next()).takeWhile(identity).map(x => row.fromRs(rs))

    def query[A](query: String)(using row: Row[A]): Iterator[A] =
      val stmt = con.prepareStatement(query)
      val rs = stmt.executeQuery()
      Iterator.continually(rs.next()).takeWhile(identity).map(x => row.fromRs(rs))