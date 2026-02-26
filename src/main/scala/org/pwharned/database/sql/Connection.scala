package org.pwharned.database.sql
import scala.util.Using
import org.pwharned.database.derive.{
  InsertBinder,
  PrimaryKeyBinder,
  PrimaryKeyFields,
  SqlDelete,
  SqlInsert,
  SqlSelect,
  SqlUpdate,
  UpdateBinder
}
import org.pwharned.database.sql.{FieldBinder, Row}

import scala.concurrent.{ExecutionContext, Future}
object Connection:
  extension (con: java.sql.Connection)

    def streamQuery[A <: Product](batchSize: Int)(using
        ec: ExecutionContext,
        sqlSelect: SqlSelect[A],
        row: Row[A]
    ): java.sql.Connection => Future[Iterator[Seq[A]]] = _ =>
      Future {
        val stmt = con.prepareStatement(sqlSelect.select)
        val rs = stmt.executeQuery()

        // This one is special: you are grouping eagerly,
        // so you must close resources manually when exhausted.
        new Iterator[Seq[A]]:
          private var nextRow: Boolean = rs.next()
          private var closed = false

          private def close(): Unit =
            if !closed then
              try rs.close()
              catch
                case _ =>
                  try stmt.close()
                  catch
                    case _ =>
                      closed = true

          override def hasNext: Boolean =
            if !nextRow then close()
            nextRow

          override def next(): Seq[A] =
            if !hasNext then throw new NoSuchElementException
            val batch = Iterator
              .continually {
                val v = row.fromRs(rs)
                nextRow = rs.next()
                v
              }
              .takeWhile(_ => nextRow)
              .take(batchSize)
              .toList

            if !nextRow then close()
            batch
      }

    def update[A <: Product, B <: Product: Row](obj: A)(using
        sqlUpdate: SqlUpdate[A],
        fb: UpdateBinder[A],
        pkb: PrimaryKeyBinder[A],
        sqlSelect: SqlSelect[B],
        row: Row[B]
    ): Iterator[B] =
      val sql = sqlUpdate.sql(obj)
      val stmt = con.prepareStatement(sql)
      fb.bind(stmt, 1, obj)
      val rs = stmt.executeQuery()
      new ResultSetIterator(rs, stmt, row)

    def update[A <: Product, B <: Product: Row](
        obj: A,
        b: PrimaryKeyFields[B]#Out
    )(using
        sqlUpdate: SqlUpdate[A],
        fb: UpdateBinder[A],
        pkb: PrimaryKeyBinder[B],
        row: Row[B]
    ): Iterator[B] =
      val sql = sqlUpdate.sql(obj)
      val stmt = con.prepareStatement(sql)
      val end = fb.bind(stmt, 1, obj)
      pkb.bind(stmt, end, b)
      val rs = stmt.executeQuery()
      new ResultSetIterator(rs, stmt, row)

    def delete[A <: Product: Row](obj: PrimaryKeyFields[A]#Out)(using
        row: Row[A],
        sqlDelete: SqlDelete[A],
        pkb: PrimaryKeyBinder[A]
    ): Iterator[A] =
      val stmt = con.prepareStatement(sqlDelete.sql)
      pkb.bind(stmt, 1, obj)
      val rs = stmt.executeQuery()
      new ResultSetIterator(rs, stmt, row)

    def insert[A <: Product, B <: Product](obj: A)(using
        ib: InsertBinder[A],
        sqlInsert: SqlInsert[A],
        sqlSelect: SqlSelect[B],
        row: Row[B]
    ): Iterator[B] =
      val stmt = con.prepareStatement(sqlInsert.sql(obj))
      ib.bind(stmt, 1, obj)
      val hasResult = stmt.execute()
      if hasResult then
        val rs = stmt.getResultSet()
        new ResultSetIterator(rs, stmt, row)
      else Iterator.empty

    def query[A](limit: Option[Int] = None, offset: Option[Int] = None)(using
        sql: SqlSelect[A],
        row: Row[A],
        dialect: SqlDialect
    ): Iterator[A] =
      val select = dialect.limitAndOffset(sql.select, limit, offset)
      val stmt = con.prepareStatement(select)
      val rs = stmt.executeQuery()
      new ResultSetIterator(rs, stmt, row)

    def query[A](a: PrimaryKeyFields[A]#Out)(using
        pkb: PrimaryKeyBinder[A],
        sql: SqlSelect[A],
        row: Row[A]
    ): Iterator[A] =
      val stmt = con.prepareStatement(sql.selectWhere)
      pkb.bind(stmt, 1, a)
      val rs = stmt.executeQuery()
      new ResultSetIterator(rs, stmt, row)

    def queryRaw[A <: Product](using
        sql: SqlSelect[A]
    ): Iterator[java.sql.ResultSet] =
      val stmt = con.prepareStatement(sql.select)
      val rs = stmt.executeQuery()

      new Iterator[java.sql.ResultSet]:
        private var hasNextRow = rs.next()
        private var closed = false

        private def close(): Unit =
          if !closed then
            try rs.close()
            catch
              case _ =>
                try stmt.close()
                catch
                  case _ =>
                    closed = true

        override def hasNext =
          if !hasNextRow then close()
          hasNextRow

        override def next() =
          if !hasNext then throw new NoSuchElementException
          val r = rs
          hasNextRow = rs.next()
          if !hasNextRow then close()
          r

    def queryParameterized[A <: Product, B <: Product: Row](
        obj: A,
        limit: Option[Int] = None,
        offset: Option[Int] = None
    )(using
        sqlSelect: SqlSelect[A],
        fb: FieldBinder[A],
        row: Row[B],
        dialect: SqlDialect
    ): Iterator[B] =
      val base = sqlSelect.selectWhere(obj)
      val select = dialect.limitAndOffset(base, limit, offset)
      val stmt = con.prepareStatement(select)
      fb.bind(stmt, 1, obj)
      val rs = stmt.executeQuery()
      new ResultSetIterator(rs, stmt, row)
