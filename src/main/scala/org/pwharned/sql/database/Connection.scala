package org.pwharned.sql.database

import org.pwharned.sql.derive.{PrimaryKeyFields, SqlDelete, SqlInsert, SqlSelect, SqlUpdate}

import scala.compiletime.summonInline
import scala.concurrent.{ExecutionContext, Future}
import org.pwharned.sql.database.Row

object Connection:
  extension (con: java.sql.Connection)
    def streamQuery[A <: Product](batchSize: Int)(using ec: ExecutionContext, sqlSelect: SqlSelect[A], row: Row[A]): java.sql.Connection => Future[Iterator[Seq[A]]] = con => Future{
  
      val stmt = con.prepareStatement(sqlSelect.select)
      val rs = stmt.executeQuery()
      
  
      Iterator.continually(rs.next())
        .takeWhile(identity)
        .map(x => row.fromRs(rs) ).grouped(batchSize)
    }
    def update[A <: Product: Row](obj: A)(using sqlSelect: SqlSelect[A], sqlUpdate: SqlUpdate[A], row:Row[A]): Iterator[A] =
  
      val stmt = con.prepareStatement(sqlUpdate.updateStatement(obj))
      sqlUpdate.bindValues(obj).zipWithIndex.foreach { case (value, index) =>
        stmt.setObject(index + 1, value) // Bind each parameter safely
      }
      val rs = stmt.executeQuery()
      Iterator.continually(rs.next())
        .takeWhile(identity)
        .map(x => row.fromRs(rs) )
  
    def update[A <: Product, B<:Product: Row](obj: A, b: PrimaryKeyFields[A]#Out)(using sqlUpdate: SqlUpdate[A], sqlSelect: SqlSelect[B], row: Row[B]): Iterator[B] =
      
      val stmt = con.prepareStatement(sqlUpdate.updateStatement(obj))
      sqlUpdate.bindValues(obj, b).zipWithIndex.foreach { case (value, index) =>
        stmt.setObject(index + 1, value) // Bind each parameter safely
      }
      val rs = stmt.executeQuery()
      Iterator.continually(rs.next())
        .takeWhile(identity)
        .map(x => row.fromRs(rs) )  
  
  
  
    def delete[A <: Product](obj: PrimaryKeyFields[A]#Out)(using sqlDelete: SqlDelete[A]): Iterator[A] =
  
     
      val stmt = con.prepareStatement(sqlDelete.deleteStatement)
      sqlDelete.bindValues(obj).zipWithIndex.foreach { case (value, index) =>
        stmt.setObject(index + 1, value) // Bind each parameter safely
      }
      val rs = stmt.executeUpdate()
      Iterator.empty
  
  
  
    def insert[A <: Product, B<:Product](obj: A)(using fb: FieldBinder[A],sqlInsert: SqlInsert[A], sqlSelect: SqlSelect[B],row: Row[B]): Iterator[B] =
  
      
  
      val built = sqlInsert.insertReturning(obj)
      val stmt = con.prepareStatement(built)
      fb.bind(stmt, 1, obj)
  
      val rs = stmt.executeQuery()
      Iterator.continually(rs.next())
        .takeWhile(identity)
        .map(x => row.fromRs(rs) )
  
    def query[A <: Product](using sql: SqlSelect[A], row:Row[A]): Iterator[A] =
      val stmt = con.prepareStatement(sql.select)
      val rs = stmt.executeQuery()
      Iterator.continually(rs.next()).takeWhile(identity).map(x => row.fromRs(rs) )
    def query[A <: Product](a:PrimaryKeyFields[A]#Out)(using sql: SqlSelect[A], row: Row[A]): Iterator[A] =
      val stmt = con.prepareStatement(sql.selectWhere)
      val bindValues = sql.bindValues(a)
      bindValues.zipWithIndex.foreach { case (value, index) =>
        stmt.setObject(index + 1, value) // Bind each parameter safely
      }
      val rs = stmt.executeQuery()
      Iterator.continually(rs.next()).takeWhile(identity).map(x => row.fromRs(rs) )
    def queryParameterized[A <: Product,B<:Product](a:A )(using sqlSelect: SqlSelect[A], row: Row[B]): Iterator[B] =

      val stmt = con.prepareStatement(sqlSelect.selectWhere (a) )
      val bindValues = sqlSelect.bindValuesOb(a)
      bindValues.zipWithIndex.foreach { case (value, index) =>
        stmt.setObject(index + 1, value) // Bind each parameter safely
      }
      val rs = stmt.executeQuery()
      Iterator.continually(rs.next()).takeWhile(identity).map(x => row.fromRs(rs) )
  
