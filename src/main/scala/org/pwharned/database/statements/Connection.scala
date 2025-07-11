package org.pwharned.database.statements

import scala.compiletime.summonInline
import scala.concurrent.{ExecutionContext, Future}

extension (rs: java.sql.ResultSet)
  transparent inline def as[A <: Product](using sql: SqlSelect[A]): A =
    sql.fromResultSet(rs)


extension (con: java.sql.Connection)
  inline def streamQuery[A <: Product](batchSize: Int)(using ec: ExecutionContext): java.sql.Connection => Future[Iterator[Seq[A]]] = con => Future{
    given sql: SqlSelect[A] = summonInline[SqlSelect[A]]

    val stmt = con.prepareStatement(sql.select)
    val rs = stmt.executeQuery()

    Iterator.continually(rs.next())
      .takeWhile(identity)
      .map(x => rs.as[A]).grouped(batchSize)
  }
  inline def update[A <: Product](obj: A): Iterator[A] =
    val sqlUpdate = summonInline[SqlUpdate[A]]
    given sqlSelect: SqlSelect[A] = summonInline[SqlSelect[A]]
    val stmt = con.prepareStatement(sqlUpdate.updateStatement(obj))
    sqlUpdate.bindValues(obj).zipWithIndex.foreach { case (value, index) =>
      stmt.setObject(index + 1, value) // Bind each parameter safely
    }
    val rs = stmt.executeQuery()
    Iterator.continually(rs.next())
      .takeWhile(identity)
      .map(x => rs.as[A])

  inline def update[A <: Product, B<:Product](obj: A, b: PrimaryKeyFields[A]#Out): Iterator[B] =
    given sqlUpdate: SqlUpdate[A] = summonInline[SqlUpdate[A]]
    given sqlSelect: SqlSelect[B] = summonInline[SqlSelect[B]]

    val stmt = con.prepareStatement(sqlUpdate.updateStatement(obj))
    sqlUpdate.bindValues(obj, b).zipWithIndex.foreach { case (value, index) =>
      stmt.setObject(index + 1, value) // Bind each parameter safely
    }
    val rs = stmt.executeQuery()
    Iterator.continually(rs.next())
      .takeWhile(identity)
      .map(x => rs.as[B])



  inline def delete[A <: Product](obj: PrimaryKeyFields[A]#Out): Iterator[A] =

    given sqlDelete: SqlDelete[A] = summonInline[SqlDelete[A]]

    given sqlSelect: SqlSelect[A] = summonInline[SqlSelect[A]]
    val stmt = con.prepareStatement(sqlDelete.deleteStatement)
    sqlDelete.bindValues(obj).zipWithIndex.foreach { case (value, index) =>
      stmt.setObject(index + 1, value) // Bind each parameter safely
    }
    val rs = stmt.executeUpdate()
    Iterator.empty



  inline def insert[A <: Product, B<:Product](obj: A): Iterator[B] =

    given fb: FieldBinder[A] = summonInline[FieldBinder[A]]
    given sqlInsert: SqlInsert[A] = summonInline[SqlInsert[A]]
    given sqlSelect: SqlSelect[B] = summonInline[SqlSelect[B]]

    val built = sqlInsert.insertReturning(obj)
    val stmt = con.prepareStatement(built)
    fb.bind(stmt, 1, obj)

    val rs = stmt.executeQuery()
    Iterator.continually(rs.next())
      .takeWhile(identity)
      .map(x => rs.as[B])
  inline def query[A <: Product]: Iterator[A] =
    given sql: SqlSelect[A] = summonInline[SqlSelect[A]]
    val stmt = con.prepareStatement(sql.select)
    val rs = stmt.executeQuery()
    Iterator.continually(rs.next()).takeWhile(identity).map(x => rs.as[A])

  inline def query[A <: Product](a:PrimaryKeyFields[A]#Out): Iterator[A] =

    given sql: SqlSelect[A] = summonInline[SqlSelect[A]]
    val stmt = con.prepareStatement(sql.selectWhere)
    val bindValues = sql.bindValues(a)
    bindValues.zipWithIndex.foreach { case (value, index) =>
      stmt.setObject(index + 1, value) // Bind each parameter safely
    }
    val rs = stmt.executeQuery()
    Iterator.continually(rs.next()).takeWhile(identity).map(x => rs.as[A])

  inline def queryParameterized[A <: Product, B<:Product](a:A ): Iterator[B] =
    given sql: SqlSelect[B] = summonInline[SqlSelect[B]]
    given  sqls: SqlSelect[A] = summonInline[SqlSelect[A]]
    val stmt = con.prepareStatement(sqls.selectWhere (a) )
    val bindValues = sqls.bindValuesOb(a)
    bindValues.zipWithIndex.foreach { case (value, index) =>
      stmt.setObject(index + 1, value) // Bind each parameter safely
    }
    val rs = stmt.executeQuery()
    Iterator.continually(rs.next()).takeWhile(identity).map(x => rs.as[B])

