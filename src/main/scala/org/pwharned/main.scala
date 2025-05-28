package org.pwharned

import generated.{user, PrimaryKey}
import org.pwharned.macros.{Db2TypeMapper, DbTypeMapper, RandomGenerator, classFieldTypes, createTable, createTableAsync, insertAsync, query, select, update, seraialize, streamQuery}


import scala.concurrent.duration.*
import scala.compiletime.{erasedValue, summonInline}
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
def logMemoryUsage(): Unit = {
  val runtime = Runtime.getRuntime
  val usedMemory = (runtime.totalMemory - runtime.freeMemory) / (1024 * 1024) // Convert to MB
  println(s"Used Memory: ${usedMemory} MB")
}
given db:DbTypeMapper = Db2TypeMapper
// Example usage within your streaming query
def getDbConnection(): java.sql.Connection = {
  val url = "jdbc:db2://localhost:50000/BLUDB"
  val user = "db2inst1"
  val password = "password"

  Class.forName("com.ibm.db2.jcc.DB2Driver") // Load DB2 JDBC driver
  java.sql.DriverManager.getConnection(url, user, password)
}

def logExecutionTime[A](query: java.sql.Connection => Iterator[Seq[A]], conn: java.sql.Connection): Iterator[Seq[A]] =
  val startTime = System.nanoTime()

  val result = query(conn) // Execute the query

  val endTime = System.nanoTime()
  val totalTimeMs = (endTime - startTime) / 1_000_000 // Convert nanoseconds to milliseconds

  println(s"Total execution time: $totalTimeMs ms")

  result // Return the it
@main
def main(): Unit =
  val u = user(1, "Aice", None)
  val conn = getDbConnection()

  Await.result(conn.createTableAsync[user], 10.seconds)






