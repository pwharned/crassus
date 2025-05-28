package org.pwharned

import generated.user
import generated.PrimaryKey
import org.pwharned.macros.{Db2TypeMapper, DbTypeMapper, RandomGenerator, classFieldTypes, createTable, createTableAsync,deleteAsync, insertAsync, query, update,select, PrimaryKeyFields,updateAsync, bindValues ,seraialize, streamQuery}

import scala.concurrent.duration.*
import scala.compiletime.{erasedValue, summonInline}
import scala.concurrent.Await
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global

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
      val u = summon[RandomGenerator[user]].generate
      val u2 = summon[RandomGenerator[user]].generate
      val u3 = user(u.id, u2.name, u2.test)

      val r: user  = Await.result(conn.insertAsync[user](u), 10.seconds).next()

      println("Created user : " + r.toString)

      println("Updated user : " + u3.toString)
      println("Update statement: " + u3.update)
      println("Bind values: " + u3.bindValues )
      user

      val r2: user = Await.result(conn.updateAsync[user](u3), 10.seconds).next()
      println(r2)
      assert(r!=r2)
      type DebugUserPrimaryKeys = PrimaryKeyFields[user]#Out
      println(summon[DebugUserPrimaryKeys[user]])

      val userPrimaryKey: PrimaryKeyFields[user]#Out = Tuple1(PrimaryKey(true)) // Example instance
      val r4: user = Await.result(conn.deleteAsync[user]( userPrimaryKey), 10.seconds).next()


      println("Executed insert and update")

    }
  }
  val userStream = conn.streamQuery[user](batchSize = 5000).apply(conn)

  val startTime = System.nanoTime()
  userStream.foreach { batch =>
    batch.foreach( x=> println(x.seraialize))
  }

