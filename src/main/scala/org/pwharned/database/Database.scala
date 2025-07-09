package org.pwharned.database

import org.pwharned.database.statements.{FieldBinder, PrimaryKeyFields, SqlDelete, SqlInsert, SqlSchema, SqlSelect, SqlUpdate}
import org.pwharned.json.JsonSerializer

import scala.compiletime.summonInline
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try


case class ConnectionDetails(url: String, user: String, password: String, cls: String)

object Database:
  given db:DbTypeMapper = Db2TypeMapper
  // Example usage within your streaming query
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


extension (rs: java.sql.ResultSet)
  inline def as[A <: Product](using sql: SqlSelect[A]): A =
    sql.fromResultSet(rs)

extension (db: org.pwharned.database.Database.type )
  inline def retrieve[T <: Product](  using ec: scala.concurrent.ExecutionContext): Future[Try[Iterator[T]]] =
    
    db.pool.withConnection {

      x => x.query[T]
    }

  inline def retrieve[A<: Product](a: PrimaryKeyFields[A]#Out)( using ec: scala.concurrent.ExecutionContext): Future[Try[Iterator[A]]] =
    db.pool.withConnection {

      x => x.query[A](a)
    }
  inline def retrieveParameterized[A <: Product, B<:Product](a: A)(using ec: scala.concurrent.ExecutionContext): Future[Try[Iterator[B]]] =
    db.pool.withConnection {

      x => x.queryParameterized[A, B](a)
    }
  inline def create[A<:Product, B<: Product](a: A)(using  ec: scala.concurrent.ExecutionContext): Future[Try[Iterator[B]]] =
    
    db.pool.withConnection {

      x => x.insert[A, B](a)
    }
  inline def delete[A <: Product](a: PrimaryKeyFields[A]#Out)(using ec: scala.concurrent.ExecutionContext): Future[Try[Iterator[A]]] =
    db.pool.withConnection {

      x => x.delete[A](a)
    }
    
  inline def update[A <: Product](a: A)(using  ec: scala.concurrent.ExecutionContext): Future[Try[Iterator[A]]] =
    db.pool.withConnection {

      x =>x.update[A](a)
    }
  inline def update[A <: Product, B<:Product](a: A, b: PrimaryKeyFields[A]#Out)(using ec: scala.concurrent.ExecutionContext): Future[Try[Iterator[B]]] =
    db.pool.withConnection {

      x => x.update[A,B](a,b)
    }

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

  inline def updateAsync[A <: Product](obj: A)(using  ec: ExecutionContext): Future[Iterator[A]] =
    
    val sqlUpdate: SqlUpdate[A] = summonInline[SqlUpdate[A]]
    given sqlSelect: SqlSelect[A] = summonInline[SqlSelect[A]]

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

  inline def insertAsync[A <: Product](obj: A)(using ec: ExecutionContext): Future[Iterator[A]] =

    val sqlInsert: SqlInsert[A] = summonInline[SqlInsert[A]]
    val fb: FieldBinder[A] = summonInline[FieldBinder[A]]

    given sqlSelect: SqlSelect[A] = summonInline[SqlSelect[A]]
    Future {
      val built = sqlInsert.insertReturning(obj)
      val stmt = con.prepareStatement(built)
      fb.bind(stmt, 1, obj)
      stmt.executeUpdate()

      val rs = stmt.executeQuery()

      Iterator.continually(rs.next())
        .takeWhile(identity)
        .map( x => rs.as[A])
    }.recover {
      case ex: Exception =>
        println(s"⚠️ Insert failed: ${ex.getMessage} ")
        Iterator.empty[A]
    }
  inline def delete[A <: Product](obj: PrimaryKeyFields[A]#Out): Iterator[A] =

    given sqlDelete: SqlDelete[A] = summonInline[SqlDelete[A]]

    given sqlSelect: SqlSelect[A] = summonInline[SqlSelect[A]]
    val stmt = con.prepareStatement(sqlDelete.deleteStatement)
    sqlDelete.bindValues(obj).zipWithIndex.foreach { case (value, index) =>
      stmt.setObject(index + 1, value) // Bind each parameter safely
    }
    val rs = stmt.executeUpdate()
    Iterator.empty

  inline def deleteAsync[A <: Product](obj: PrimaryKeyFields[A]#Out)(using ec: ExecutionContext): Future[Iterator[A]] =
    
    val sqlDelete: SqlDelete[A] = summonInline[SqlDelete[A]]

    val sqlSelect: SqlSelect[A] = summonInline[SqlSelect[A]]
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

  inline def createTableAsync[A <: Product]( using ec: ExecutionContext, db: DbTypeMapper): Future[Unit] =
    val schema = summonInline[SqlSchema[A]]

    Future {
        val stmt = con.prepareStatement(schema.createTable(db))
        stmt.executeUpdate()
        println(s"Succesfully created table: ${schema.createTable}")
      }.recover {
        case ex: Exception =>
          println(s"⚠️ Error creating table: ${ex.getMessage} : ${schema.createTable}")
      }
  inline def createTable[A <: Product](using ec: ExecutionContext, db: DbTypeMapper): Unit =
    val schema = summonInline[SqlSchema[A]]
    val stmt = con.prepareStatement(schema.createTable(db))
    stmt.executeUpdate()
    println(s"Succesfully created table: ${schema.createTable}")