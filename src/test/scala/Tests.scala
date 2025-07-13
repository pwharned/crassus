package org.pwharned

import org.pwharned.sql.database.HKD.*
import org.pwharned.sql.*
import org.pwharned.sql.database.Connection.*
import org.pwharned.json.serialize
import org.pwharned.sql.database.{Db2TypeMapper, DbTypeMapper}
import org.pwharned.sql.database.FieldBinder.*
import org.pwharned.sql.derive.{PrimaryKeyExtractor, PrimaryKeyFields, PrimaryKeyParser, SqlInsert, SqlSelect, TupleKeyExtractor}
import org.pwharned.sql.dialect.{Db2Dialect, PostgresDialect, SqlDialect}
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
  val url = "jdbc:postgresql://localhost:5433/postgres"
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


  // Ensure table exists (for testing)
  conn.createTable[users[Id]]
  

  (0 to 5).iterator.foreach {
    x => {
      val user1: New[users] = RandomGenerator[New[users]].generate
      println(summon[SqlInsert[New[users]]].sql(user1))
      val user2: Persisted[users] = conn.insert[New[users], Persisted[users]](user1).next()


      val user3: Persisted[users] = Randomizer.derived[Persisted[users]].randomize(user2)
      println(PrimaryKeyExtractor.getPrimaryKey[Persisted[users]])
      val pkeys: PrimaryKeyFields[Persisted[users]]#Out = TupleKeyExtractor.extractPkTuple(user3).asInstanceOf[PrimaryKeyFields[Persisted[users]]#Out]
      println(user3)
      val user4: Persisted[users] = conn.update[Persisted[users], Persisted[users]](user3,pkeys).next() // update
      //assert(user4!=user3) // assert that the update user is different

    }
  }
  val userStream = conn.query[Persisted[users]]// select the updated values

  val startTime = System.nanoTime()
  userStream.foreach( x =>
{
 val pkeys: PrimaryKeyFields[Persisted[users]]#Out = TupleKeyExtractor.extractPkTuple(x).asInstanceOf[PrimaryKeyFields[Persisted[users]]#Out]
  val pkeystring: Seq[String] = pkeys.productIterator.toSeq.map {
    case p: PrimaryKey[?] => p.value.toString
  }
  println("The primaryKey string is : " + pkeystring)
  val parseKeys = PrimaryKeyParser.makeParser[Persisted[users]]
  val keyTuple: PrimaryKeyFields[Persisted[users]]#Out = parseKeys(pkeystring)
  println("The extracted key key tuple is" +  keyTuple)
  keyTuple.productIterator.foreach{
    case x: PrimaryKey[?] => println(x.value)
  }
  val r4 = conn.query[Persisted[users]](keyTuple).next()
  println(r4)
  val r5 = conn.delete[Persisted[users]](keyTuple)
    }
)


  

  //val finalUsers = conn.streamQuery[users[Id]](batchSize = 5000).apply(conn)
  //println( (System.nanoTime() - startTime)/ 1000000)
