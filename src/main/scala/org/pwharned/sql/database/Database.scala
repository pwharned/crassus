package org.pwharned.sql.database

import org.pwharned.json.JsonSerializer
import org.pwharned.sql.database.{ConnectionPool, FieldBinder}
import org.pwharned.sql.dialect.SqlDialect
import org.pwharned.sql.statements.*

import java.sql.Connection
import scala.compiletime.summonInline
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try


case class ConnectionDetails(url: String, user: String, password: String, cls: String)

class Database(using sqlDial: SqlDialect, mapper: DbTypeMapper) {
  def getDbConnection(con: ConnectionDetails): java.sql.Connection = {
    val url = con.url
    val user = con.user
    val password = con.password

    Class.forName(con.cls) // Load DB2 JDBC driver
    java.sql.DriverManager.getConnection(url, user, password)
  }
  var pool: ConnectionPool = null;

  def createPool(con: ConnectionDetails): Unit = {
    pool =  new ConnectionPool(con.cls,con.url,con.user, con.password)

  }

  def withConnection[T](f: Connection => T)(using ec: scala.concurrent.ExecutionContext): Future[Try[T]] = {
    pool.withConnection(f)
  }
  }


  

object Database:
  def apply(con: ConnectionDetails)(using sqldial: SqlDialect, typeMapper: DbTypeMapper): Database = {
    val db = new Database()
    db.createPool(con)
    db
  }

