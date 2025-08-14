package org.pwharned.sql.database

import org.pwharned.sql.database.Row
import org.pwharned.sql.derive.{InsertBinder, PrimaryKeyBinder, PrimaryKeyFields, SqlDelete, SqlInsert, SqlSchema, SqlSelect, SqlUpdate, UpdateBinder}

import scala.concurrent.{ExecutionContext, Future}

object Connection:
  extension (con: java.sql.Connection)
    def streamQuery[A <: Product](batchSize: Int)(using ec: ExecutionContext, sqlSelect: SqlSelect[A], row: Row[A]): java.sql.Connection => Future[Iterator[Seq[A]]] = con => Future{
  
      val stmt = con.prepareStatement(sqlSelect.select)
      val rs = stmt.executeQuery()
      
  
      Iterator.continually(rs.next())
        .takeWhile(identity)
        .map(x => row.fromRs(rs) ).grouped(batchSize)
    }

    def update[A <: Product, B <: Product : Row](obj: A)(using sqlUpdate: SqlUpdate[A], fb: UpdateBinder[A], pkb: PrimaryKeyBinder[A], sqlSelect: SqlSelect[B], row: Row[B]): Iterator[B] =

      val sql = sqlUpdate.sql(obj)
      val stmt = con.prepareStatement(sql)

      val end = fb.bind(stmt, 1, obj)

      val rs = stmt.executeQuery()
      Iterator.continually(rs.next())
        .takeWhile(identity)
        .map(x => row.fromRs(rs))

    def update[A <: Product, B<:Product: Row](obj: A, b: PrimaryKeyFields[B]#Out)(using sqlUpdate: SqlUpdate[A],fb: UpdateBinder[A], pkb: PrimaryKeyBinder[B], row: Row[B]): Iterator[B] =

      val sql = sqlUpdate.sql(obj)
      val stmt = con.prepareStatement(sql)
      val end = fb.bind(stmt, 1, obj)

      val end2 = pkb.bind(stmt, end, b)
      val rs = stmt.executeQuery()
      Iterator.continually(rs.next())
        .takeWhile(identity)
        .map(x => row.fromRs(rs) )



    def delete[A <: Product: Row](obj: PrimaryKeyFields[A]#Out)(using row: Row[A], sqlDelete: SqlDelete[A],pkb: PrimaryKeyBinder[A]): Iterator[A] =

      val sql = sqlDelete.sql
      val stmt = con.prepareStatement(sql)
      val end2 = pkb.bind(stmt, 1, obj)
      val rs = stmt.executeQuery()
      Iterator.continually(rs.next())
        .takeWhile(identity)
        .map(x => row.fromRs(rs))

  
  
    def insert[A <: Product, B<:Product](obj: A)(using ib: InsertBinder[A],sqlInsert: SqlInsert[A], sqlSelect: SqlSelect[B],row: Row[B]): Iterator[B] =
      val built = sqlInsert.sql(obj)
      println(built)
      println(obj)
      val stmt = con.prepareStatement(built)
      val bound = ib.bind(stmt, 1, obj)
      val rs = stmt.executeQuery()

      Iterator.continually(rs.next())
        .takeWhile(identity)
        .map(x => row.fromRs(rs) )

    def query[A <: Product](using sql: SqlSelect[A], row:Row[A]): Iterator[A] =
      val stmt = con.prepareStatement(sql.select)
      val rs = stmt.executeQuery()
      Iterator.continually(rs.next()).takeWhile(identity).map(x => row.fromRs(rs) )
    def query[A <: Product](a:PrimaryKeyFields[A]#Out)(using pkb: PrimaryKeyBinder[A],sql: SqlSelect[A], row: Row[A]): Iterator[A] =

      val stmt = con.prepareStatement(sql.selectWhere)
      val bindValues = pkb.bind(stmt, 1, a)

      val rs = stmt.executeQuery()
      Iterator.continually(rs.next()).takeWhile(identity).map(x => row.fromRs(rs) )
    def queryRaw[A <: Product](using  sql: SqlSelect[A]): Iterator[java.sql.ResultSet] =

      val stmt = con.prepareStatement(sql.select)

      val rs = stmt.executeQuery()
      Iterator.continually(rs.next()).takeWhile(identity).map(x => rs)
      
    def queryParameterized[A <: Product,B<:Product: Row](obj:A )(using sqlSelect: SqlSelect[A],fb:FieldBinder[A], row: Row[B]): Iterator[B] =
      val sql = sqlSelect.selectWhere(obj)

      val stmt = con.prepareStatement(sql )
      val end = fb.bind(stmt, 1, obj)

      val rs = stmt.executeQuery()
      Iterator.continually(rs.next()).takeWhile(identity).map(x => row.fromRs(rs) )

    def createTable[A <: Product](using schema: SqlSchema[A], ec: ExecutionContext, db: DbTypeMapper): Unit =

      val stmt = con.prepareStatement(schema.createTable(db))

      stmt.executeUpdate()