package org.pwharned.macros

import org.pwharned.http.HttpPath.HttpPath
import org.pwharned.http.generated.Headers
import org.pwharned.http.{HttpPath, HttpRequest, HttpResponse}
import org.pwharned.macros
import org.pwharned.parse.ParseError

import scala.collection.Iterator
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

extension [T<:Product](entity: T)(using sql: SqlSelect[T])
  def fields: List[String] = summon[SqlSelect[T]].names
  def select: String = summon[SqlSelect[T]].select
  def classFieldTypes: List[String] = summon[SqlSelect[T]].getClassesFieldType
extension[T <: Product] (entity: T) (using sql: SqlUpdate[T], sqlDelete: SqlDelete[T] )
  def update: String = summon[SqlUpdate[T]].updateStatement(entity)
  def bindValues: Seq[Any] = summon[SqlUpdate[T]].bindValues(entity)
  def values(l:List[String]): PrimaryKeyFields[T]#Out = summon[SqlDelete[T]].values(l)




extension[T: SqlSchema] (t: T) def createTable(using db:DbTypeMapper): String = summon[SqlSchema[T]].createTable(db)
extension (inline s: String) inline def asPath: HttpPath = HttpPath.literal(s)
extension ( s: String)  def toPath: HttpPath = HttpPath(s)

extension (b: java.nio.ByteBuffer) def asRequest: Option[HttpRequest.HttpRequest] = HttpRequest.HttpRequest.fromFullBuffer(b)

extension (rs: java.sql.ResultSet)
  inline def as[A <: Product](using sql: SqlSelect[A]): A =
    sql.fromResultSet(rs)

extension[T<:Product](obj: T) (using json: JsonSerializer[T])
  inline def serialize: String = summon[JsonSerializer[T]].serialize(obj)
extension[T <: Product] (obj: Iterator[T]) (using json: JsonSerializer[T] )
  inline def serialize: String = summon[JsonSerializer[T]].serialize(obj)



extension (db: org.pwharned.database.Database.type )
  inline def retrieve[A <: Product](using sql: SqlSelect[A], json:JsonSerializer[A],  ec: scala.concurrent.ExecutionContext): Future[HttpResponse] =
    db.pool.withConnection {

      x => HttpResponse.ok(x.query[A].serialize ,Headers.apply(Map("content-type" -> "Application/json")) )
    }.map {
      case Failure(exception) => HttpResponse.error(exception.toString)
      case Success(value) => {
        value
      }
    }
  inline def create[A <: Product](a: A)(using sql: SqlSelect[A], sqlInsert: SqlInsert[A], json: JsonSerializer[A], ec: scala.concurrent.ExecutionContext): Future[HttpResponse] =
    db.pool.withConnection {

      x => HttpResponse.ok(x.insert[A](a).serialize, headers = Headers.empty.add("content-type", "application/json"))
    }.map {
      case Failure(exception) => HttpResponse.error(exception.toString)
      case Success(value) => value
    }
  inline def delete[A <: Product](a: PrimaryKeyFields[A]#Out)(using sql: SqlSelect[A], sqlDelete: SqlDelete[A], json: JsonSerializer[A], ec: scala.concurrent.ExecutionContext): Future[HttpResponse] =
    db.pool.withConnection {

      x => HttpResponse.ok(x.delete[A](a).serialize, headers = Headers.empty.add("content-type", "application/json"))
    }.map {
      case Failure(exception) => HttpResponse.error(exception.toString)
      case Success(value) => value
    }    

extension (s: String)
  def deserialize[A <: Product](using j: JsonDeserializer[A]): Either[ParseError, A] = summon[JsonDeserializer[A]].deserialize(s)




extension (con: java.sql.Connection)
  inline def streamQuery[A <: Product](batchSize: Int)(using sql: SqlSelect[A], ec: ExecutionContext): java.sql.Connection => Future[Iterator[Seq[A]]] = con => Future{
    val stmt = con.prepareStatement(sql.select)
    val rs = stmt.executeQuery()

    Iterator.continually(rs.next())
      .takeWhile(identity)
      .map(x => rs.as[A]).grouped(batchSize)
  }
  
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
  inline def delete[A <: Product](obj: PrimaryKeyFields[A]#Out)(using sqlDelete: SqlDelete[A], sqlSelect: SqlSelect[A], ec: ExecutionContext): Iterator[A] =
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


  inline def insert[A <: Product](obj: A)(using sqlInsert: SqlInsert[A], sqlSelect: SqlSelect[A], ec: ExecutionContext): Iterator[A] =
      val stmt = con.prepareStatement(sqlInsert.insertStatement)
      sqlInsert.bindValues(obj).zipWithIndex.foreach { case (value, index) =>
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