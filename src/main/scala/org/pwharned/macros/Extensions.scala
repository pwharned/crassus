package org.pwharned.macros

import org.pwharned.macros

import scala.concurrent.{ExecutionContext, Future}

extension [T<:Product](entity: T)(using sql: SqlSelect[T])
  def fields: List[String] = summon[SqlSelect[T]].names
  def select: String = summon[SqlSelect[T]].select
  def classFieldTypes: List[String] = summon[SqlSelect[T]].getClassesFieldType


extension[T <: Product] (entity: T) (using sql: RandomGenerator[T] )
  def generate: T = summon[RandomGenerator[T]].generate


extension[T: SqlSchema] (t: T) def createTable(using db:DbTypeMapper): String = summon[SqlSchema[T]].createTable(db)


extension (rs: java.sql.ResultSet)
  inline def as[A <: Product](using sql: SqlSelect[A]): A =
    sql.fromResultSet(rs)

extension[T<:Product](obj: T) (using json: JsonSerializer[T])
  def seraialize: String = summon[JsonSerializer[T]].serialize(obj)

extension (con: java.sql.Connection)
  inline def query[A <: Product](using sql: SqlSelect[A]): Iterator[A] =
    val stmt = con.prepareStatement(sql.select)
    val rs = stmt.executeQuery()
    Iterator.continually(rs.next()).takeWhile(identity).map( x => rs.as[A])


extension (con: java.sql.Connection)
  def createTableAsync[A <: Product](using schema: SqlSchema[A], ec: ExecutionContext,db: DbTypeMapper): Future[Unit] =
    Future {
      val stmt = con.prepareStatement(schema.createTable(db))
      stmt.executeUpdate()
      println(s"Succesfully created table: ${schema.createTable}")
    }.recover {
      case ex: Exception =>
        println(s"⚠️ Error creating table: ${ex.getMessage} : ${schema.createTable}")
    }


extension (con: java.sql.Connection)
  inline def streamQuery[A <: Product](batchSize: Int)(using sql: SqlSelect[A]): java.sql.Connection => Iterator[Seq[A]] = con =>
    val stmt = con.prepareStatement(sql.select)
    val rs = stmt.executeQuery()

    Iterator.continually(rs.next())
      .takeWhile(identity)
      .map( x => rs.as[A]).grouped(batchSize)




extension (con: java.sql.Connection)
  def insertAsync[A <: Product](obj: A)(using sql: SqlInsert[A], ec: ExecutionContext): Future[Unit] =
    Future {
      val stmt = con.prepareStatement(sql.insertStatement)
      sql.bindValues(obj).zipWithIndex.foreach { case (value, index) =>
        stmt.setObject(index + 1, value) // Bind each parameter safely
      }
      stmt.executeUpdate()
      println(s"Insert succeeeded: ${obj.toString}")
    }.recover {
      case ex: Exception =>
        println(s"⚠️ Insert failed: ${ex.getMessage} : ${sql.insertStatement}")
    }
