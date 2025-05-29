package org.pwharned

import generated.user
import generated.PrimaryKey
import org.pwharned.macros.{Db2TypeMapper, DbTypeMapper, RandomGenerator, classFieldTypes, createTable, createTableAsync,deleteAsync, insertAsync, query, update,select, PrimaryKeyFields,updateAsync, bindValues ,seraialize, streamQuery}

import scala.concurrent.duration.*
import scala.compiletime.{erasedValue, summonInline}
import scala.concurrent.Await
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
private val executor = Executors.newVirtualThreadPerTaskExecutor()
given ExecutionContext = ExecutionContext.fromExecutor(executor) // Use virtual threads for Scala Futures


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


  // Ensure table exists (for testing)
  conn.createTable[user]

  (0 to 100).iterator.foreach {
    x => {
      val u = summon[RandomGenerator[user]].generate // generate a random user
      val u2 = summon[RandomGenerator[user]].generate // generate some random values for update
      val u3 = user(u.id, u2.name, u2.test) // new user with same id as inserted user, but different random values

      val r: user  = Await.result(conn.insertAsync[user](u), 10.seconds).next() // insert

      val r2: user = Await.result(conn.updateAsync[user](u3), 10.seconds).next() // update
      assert(r!=r2) // assert that the update user is different

    }
  }
  val userStream = conn.streamQuery[user](batchSize = 5000).apply(conn) // select the updated values

  val startTime = System.nanoTime()
  userStream.foreach { batch =>
    batch.foreach{ x=> 
      val userPrimaryKey: PrimaryKeyFields[user]#Out = Tuple1(PrimaryKey(x.id)).asInstanceOf[PrimaryKeyFields[user]#Out] // construct the primary key
      val r4 = Await.result(conn.deleteAsync[user](userPrimaryKey), 10.seconds) // delete the user
    }


  }

  val finalUsers = conn.streamQuery[user](batchSize = 5000).apply(conn)
  assert(finalUsers.isEmpty) // table should be empty
  println( (System.nanoTime() - startTime)/ 1000000)
