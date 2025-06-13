package org.pwharned

import generated.user
import org.pwharned.database.HKD.*
import org.pwharned.database.*
import org.pwharned.json.serialize

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
private val executor = Executors.newVirtualThreadPerTaskExecutor()
given ExecutionContext = ExecutionContext.fromExecutor(executor) // Use virtual threads for Scala Futures


def timed[A](block: => A): A =
  val start = System.nanoTime
  val result = block
  val end = System.nanoTime
  val elapsedMs = (end - start) / 1000000.0
  println(f"Elapsed time: $elapsedMs%.3f ms")
  result
given db: DbTypeMapper = Db2TypeMapper

def getDbConnection(): java.sql.Connection = {
  val url = "jdbc:db2://localhost:50000/BLUDB"
  val user = "db2inst1"
  val password = "password"

  Class.forName("com.ibm.db2.jcc.DB2Driver") // Load DB2 JDBC driver
  java.sql.DriverManager.getConnection(url, user, password)
}
@main
def test:Unit =
  val conn = getDbConnection()
  import scala.language.implicitConversions
  


  // Ensure table exists (for testing)
  conn.createTable[user[Id]]
  

  (0 to 100).iterator.foreach {
    x => {
      val u = summon[RandomGenerator[New[user]]].generate // generate a random user
      val u2 = summon[RandomGenerator[Updated[user]]].generate // generate some random values for update
      val r: Persisted[user] = conn.insert[New[user], Persisted[user]](u).next() // insert
      val u3: Updated[user] = user(None, u2.name, u2.test) // new user with same id as inserted user, but different random values
      val pkeys: PrimaryKeyFields[Updated[user]]#Out  = Tuple1(r.id).asInstanceOf[PrimaryKeyFields[Updated[user]]#Out]
      val r2: Updated[user] = conn.update[Updated[user]](u3,pkeys).next() // update
      assert(r!=r2) // assert that the update user is different

    }
  }
  val userStream = conn.query[user[Id]]// select the updated values

  val startTime = System.nanoTime()
  userStream.foreach( x =>
{     
      val userPrimaryKey: PrimaryKeyFields[user[Id]]#Out = Tuple1(PrimaryKey(x.id.toString)).asInstanceOf[PrimaryKeyFields[user[Id]]#Out]
      println(x.serialize)
  
  //val r4 = Await.result(conn.deleteAsync[user[Id]](userPrimaryKey), 10.seconds) // delete the user
    }
)


  

  val finalUsers = conn.streamQuery[user[Id]](batchSize = 5000).apply(conn)
  //println( (System.nanoTime() - startTime)/ 1000000)
