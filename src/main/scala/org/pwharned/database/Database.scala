package org.pwharned.database

import org.pwharned.database.HKD.Persisted
import org.pwharned.http.{Headers, HttpResponse}
import org.pwharned.json.{JsonSerializer, serialize}
import org.pwharned.macros.*

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object Database:
  given db:DbTypeMapper = Db2TypeMapper
  // Example usage within your streaming query
  def getDbConnection: java.sql.Connection = {
    val url = "jdbc:db2://localhost:50000/BLUDB"
    val user = "db2inst1"
    val password = "password"
  
    Class.forName("com.ibm.db2.jcc.DB2Driver") // Load DB2 JDBC driver
    java.sql.DriverManager.getConnection(url, user, password)
  }

  val pool = new ConnectionPool("com.ibm.db2.jcc.DB2Driver","jdbc:db2://localhost:50000/BLUDB","db2inst1", "password")


extension (rs: java.sql.ResultSet)
  inline def as[A <: Product](using sql: SqlSelect[A]): A =
    sql.fromResultSet(rs)

extension (db: org.pwharned.database.Database.type )
  inline def retrieve[T[F[_]] <: Product](using sql: SqlSelect[Persisted[T]], json:JsonSerializer[Persisted[T]],  ec: scala.concurrent.ExecutionContext): Future[Try[Iterator[Persisted[T]]]] =
    db.pool.withConnection {

      x => x.query[Persisted[T]]
      //HttpResponse.ok(x.query[A].serialize ,Headers.apply(Map("content-type" -> "Application/json")) )
    }
  inline def create[A <: Product](a: A)(using sql: SqlSelect[A], sqlInsert: SqlInsert[A], json: JsonSerializer[A], ec: scala.concurrent.ExecutionContext): Future[HttpResponse] =
    db.pool.withConnection {

      x => HttpResponse.ok(x.insert[A](a).serialize, headers = Headers.empty.add("content-type", "application/json"))
    }.map {
      case Failure(exception) => HttpResponse.error(exception.toString)
      case Success(value) => value
      case Success(value) => value
    }
  inline def delete[A <: Product](a: PrimaryKeyFields[A]#Out)(using sql: SqlSelect[A], sqlDelete: SqlDelete[A], json: JsonSerializer[A], ec: scala.concurrent.ExecutionContext): Future[HttpResponse] =
    db.pool.withConnection {

      x => HttpResponse.ok(x.delete[A](a).serialize, headers = Headers.empty.add("content-type", "application/json"))
    }.map {
      case Failure(exception) => HttpResponse.error(exception.toString)
      case Success(value) => value
    }
  inline def update[A <: Product](a: A)(using sql: SqlSelect[A], sqlUpdate: SqlUpdate[A], json: JsonSerializer[A], ec: scala.concurrent.ExecutionContext): Future[HttpResponse] =
    db.pool.withConnection {

      x => HttpResponse.ok(x.update[A](a).serialize, headers = Headers.empty.add("content-type", "application/json"))
    }.map {
      case Failure(exception) => HttpResponse.error(exception.toString)
      case Success(value) => value
    }


extension (con: java.sql.Connection)
  inline def streamQuery[A <: Product](batchSize: Int)(using sql: SqlSelect[A], ec: ExecutionContext): java.sql.Connection => Future[Iterator[Seq[A]]] = con => Future{
    val stmt = con.prepareStatement(sql.select)
    val rs = stmt.executeQuery()

    Iterator.continually(rs.next())
      .takeWhile(identity)
      .map(x => rs.as[A]).grouped(batchSize)
  }
  def update[A <: Product](obj: A)(using sqlUpdate: SqlUpdate[A], sqlSelect: SqlSelect[A]): Iterator[A] =
    val stmt = con.prepareStatement(sqlUpdate.updateStatement(obj))
    sqlUpdate.bindValues(obj).zipWithIndex.foreach { case (value, index) =>
      stmt.setObject(index + 1, value) // Bind each parameter safely
    }
    val rs = stmt.executeQuery()
    Iterator.continually(rs.next())
      .takeWhile(identity)
      .map(x => rs.as[A])


  def updateAsync[A <: Product](obj: A)(using sqlUpdate: SqlUpdate[A], sqlSelect: SqlSelect[A], ec: ExecutionContext): Future[Iterator[A]] =
    Future {
      val stmt = con.prepareStatement(sqlUpdate.updateStatement(obj))
      sqlUpdate.bindValues(obj).zipWithIndex.foreach { case (value, index) =>
        stmt.setObject(index + 1, value) // Bind each parameter safely
      }
      val rs = stmt.executeQuery()
      Iterator.continually(rs.next())
        .takeWhile(identity)
        .map(x => rs.as[A])
    }.recover {
      case ex: Exception =>
        println(s"⚠️ Insert failed: ${ex.getMessage} : ${sqlUpdate.updateStatement(obj)}")
        Iterator.empty[A]
    }

  inline def insertAsync[A <: Product](obj: A)(using sqlInsert: SqlInsert[A], sqlSelect: SqlSelect[A], ec: ExecutionContext): Future[Iterator[A]] =
    Future {
      val stmt = con.prepareStatement(sqlInsert.insertStatement)
      sqlInsert.bindValues(obj).zipWithIndex.foreach { case (value, index) =>
        stmt.setObject(index + 1, value) // Bind each parameter safely
      }
      val rs = stmt.executeQuery()

      Iterator.continually(rs.next())
        .takeWhile(identity)
        .map( x => rs.as[A])
    }.recover {
      case ex: Exception =>
        println(s"⚠️ Insert failed: ${ex.getMessage} : ${sqlInsert.insertStatement}")
        Iterator.empty[A]
    }
  inline def delete[A <: Product](obj: PrimaryKeyFields[A]#Out)(using sqlDelete: SqlDelete[A], sqlSelect: SqlSelect[A]): Iterator[A] =
    val stmt = con.prepareStatement(sqlDelete.deleteStatement)
    sqlDelete.bindValues(obj).zipWithIndex.foreach { case (value, index) =>
      stmt.setObject(index + 1, value) // Bind each parameter safely
    }
    val rs = stmt.executeUpdate()
    Iterator.empty

  inline def deleteAsync[A <: Product](obj: PrimaryKeyFields[A]#Out)(using sqlDelete: SqlDelete[A], sqlSelect: SqlSelect[A], ec: ExecutionContext): Future[Iterator[A]] =
    Future {
      val stmt = con.prepareStatement(sqlDelete.deleteStatement)
      sqlDelete.bindValues(obj).zipWithIndex.foreach { case (value, index) =>
        stmt.setObject(index + 1, value) // Bind each parameter safely
      }
      val rs = stmt.executeUpdate()
      Iterator.empty

    }.recover {
      case ex: Exception =>
        println(s"⚠️ Delete failed: ${ex.getMessage} : ${sqlDelete.deleteStatement}")
        Iterator.empty[A]
    }


  inline def insert[A <: Product](obj: A)(using sqlInsert: SqlInsert[A], sqlSelect: SqlSelect[A]): Iterator[A] =
    val stmt = con.prepareStatement(sqlInsert.insertStatement)
    val bindValues = sqlInsert.bindValues(obj)
    bindValues.zipWithIndex.foreach { case (value, index) =>
      stmt.setObject(index + 1, value) // Bind each parameter safely
    }
    val rs = stmt.executeQuery()
    Iterator.continually(rs.next())
      .takeWhile(identity)
      .map(x => rs.as[A])
  inline def query[A <: Product](using sql: SqlSelect[A]): Iterator[A] =
    val stmt = con.prepareStatement(sql.select)
    val rs = stmt.executeQuery()
    Iterator.continually(rs.next()).takeWhile(identity).map(x => rs.as[A])

  inline def createTableAsync[A <: Product](using schema: SqlSchema[A], ec: ExecutionContext, db: DbTypeMapper): Future[Unit] =
    Future {
      val stmt = con.prepareStatement(schema.createTable(db))
      stmt.executeUpdate()
      println(s"Succesfully created table: ${schema.createTable}")
    }.recover {
      case ex: Exception =>
        println(s"⚠️ Error creating table: ${ex.getMessage} : ${schema.createTable}")
    }
  inline def createTable[A <: Product](using schema: SqlSchema[A], ec: ExecutionContext, db: DbTypeMapper): Unit =
    val stmt = con.prepareStatement(schema.createTable(db))
    stmt.executeUpdate()
    println(s"Succesfully created table: ${schema.createTable}")