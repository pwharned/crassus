package org.pwharned.database.sql

import org.pwharned.database.*

import java.sql.Connection
import java.util.concurrent.{ExecutorService, Executors, Semaphore}
import scala.compiletime.summonInline
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try


case class ConnectionDetails(url: String, user: String, password: String, cls: String)

class Database(using sqlDial: SqlDialect, mapper: DbTypeMapper) {

  // dedicated executor and ec for blocking DB work
  private val executor: ExecutorService =  Executors.newVirtualThreadPerTaskExecutor()
  private given ec: ExecutionContext  = ExecutionContext.fromExecutorService(executor)
  private val concurrencyLimit: Semaphore = Semaphore(1000)

  def getDbConnection(con: ConnectionDetails): java.sql.Connection = {
    val url = con.url
    val user = con.user
    val password = con.password

    Class.forName(con.cls) // Load DB2 JDBC driver
    java.sql.DriverManager.getConnection(url, user, password)
  }

  val dial = sqlDial
  var pool: ConnectionPool = null;

  def createPool(con: ConnectionDetails): Unit = {
    pool = new ConnectionPool(con.cls, con.url, con.user, con.password)
  }

  def withConnection[T](f: java.sql.Connection => T): Future[Try[T]] = {
    // acquire permit before dispatching work so we don't exceed DB connections
    concurrencyLimit.acquire()
    try pool.withConnection { conn => f(conn) }
    finally concurrencyLimit.release()
  }

}
object Database:
  def apply(con: ConnectionDetails)(using sqldial: SqlDialect, typeMapper: DbTypeMapper): Database = {
    val db = new Database()
    db.createPool(con)
    db
  }

