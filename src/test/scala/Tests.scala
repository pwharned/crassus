package org.pwharned

import generated.assets
import org.pwharned.sql.*
import org.pwharned.sql.database.Connection.*
import org.pwharned.sql.database.HKD.*
import org.pwharned.sql.database.{Db2TypeMapper, DbTypeMapper}
import org.pwharned.sql.derive.*
import org.pwharned.sql.dialect.{PostgresDialect, SqlDialect}
import org.pwharned.sql.test.DatabaseTest
import org.pwharned.utils.{RandomGenerator, Randomizer}

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
given dial: SqlDialect = PostgresDialect

def timed[A](block: => A): A =
  val start = System.nanoTime
  val result = block
  val end = System.nanoTime
  val elapsedMs = (end - start) / 1000000.0
  println(f"Elapsed time: $elapsedMs%.3f ms")
  result


given db: DbTypeMapper = Db2TypeMapper

case class users[F[_]](id: F[PrimaryKey[Int]], name: F[Nullable[String]], test: F[String])

def getDbConnection(): java.sql.Connection = {
  val url = "jdbc:postgresql://localhost:5433/postgres?currentSchema=public"
  val user = "postgres"
  val password = "password"

  Class.forName("org.postgresql.Driver") // Load DB2 JDBC driver
  java.sql.DriverManager.getConnection(url, user, password)
}





@main
def test:Unit =
  val conn = getDbConnection()
  import scala.language.implicitConversions

  given ExecutionContext = ExecutionContext.fromExecutor(Executors.newVirtualThreadPerTaskExecutor())

  val stmt = conn.createStatement()
  val rs = stmt.executeQuery("SELECT tablename, schemaname FROM pg_tables WHERE schemaname = 'public' or tablename = 'schemaname' ")
  while (rs.next()) {
    println(rs.getString("tablename") + " " +  rs.getString("schemaname"))
  }

  // Summon for the constructor `assets`, not an applied type
  val testAssets = summon[DatabaseTest[assets]]
  testAssets.test(conn)

