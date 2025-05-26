package org.pwharned

import generated.mytablev
import org.pwharned.macros.{select, classFieldTypes,query, streamQuery}
import scala.compiletime.{erasedValue, summonInline}

def logMemoryUsage(): Unit = {
  val runtime = Runtime.getRuntime
  val usedMemory = (runtime.totalMemory - runtime.freeMemory) / (1024 * 1024) // Convert to MB
  println(s"Used Memory: ${usedMemory} MB")
}

// Example usage within your streaming query

def logExecutionTime[A](query: java.sql.Connection => Iterator[Seq[A]], conn: java.sql.Connection): Iterator[Seq[A]] =
  val startTime = System.nanoTime()

  val result = query(conn) // Execute the query

  val endTime = System.nanoTime()
  val totalTimeMs = (endTime - startTime) / 1_000_000 // Convert nanoseconds to milliseconds

  println(s"Total execution time: $totalTimeMs ms")

  result // Return the it
@main
def main(): Unit =
  val user = mytablev(1, "Aice")
  println(user.select)
  val test = user.classFieldTypes
  println(test)

  def getDbConnection(): java.sql.Connection = {
    val url = "jdbc:db2://localhost:50000/BLUDB"
    val user = "db2inst1"
    val password = "password"

    Class.forName("com.ibm.db2.jcc.DB2Driver") // Load DB2 JDBC driver
    java.sql.DriverManager .getConnection(url, user, password)
  }

    val conn = getDbConnection()

    // Ensure table exists (for testing)
    val createStatement = conn.createStatement()
    createStatement.executeUpdate(
      """CREATE TABLE IF NOT EXISTS mytable (
        |id INT,
        |name VARCHAR(100)
        |)""".stripMargin
    )

    // Insert test data
    //createStatement.executeUpdate("INSERT INTO mytable (id, name) VALUES (1, 'Alice')")
    val userStream = logExecutionTime(conn.streamQuery[mytablev](batchSize = 5000), conn)
  val startTime = System.nanoTime()
    userStream.foreach { batch =>
      batch.foreach( _ => ())
    }
  val endTime = System.nanoTime()
  val totalTimeSec = (endTime - startTime) / 1_000_000_000.0 // Convert to seconds (as a floating-point number)

  println(f"Total execution time: $totalTimeSec%.3f seconds") // Format to 3 decimal places

// Run query and convert results to case class

  //println(test)  // Output: List("Int", "String", "Option[String]", "Boolean")







