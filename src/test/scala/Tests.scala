package org.pwharned

import generated.*
import org.pwharned.sql.*
import org.pwharned.sql.database.Connection.*
import org.pwharned.sql.HKD._
import org.pwharned.sql.database.{Db2TypeMapper, DbTypeMapper}
import org.pwharned.sql.derive.*
import org.pwharned.sql.dialect.{PostgresDialect, SqlDialect}
import org.pwharned.sql.test.{DatabaseTest, DatabaseTestNoUpdate}
import org.pwharned.utils.{RandomGenerator, RandomValue, Randomizer}

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
  given RandomValue[Option[Vector[Float]]] with
    def generate: Option[Vector[Float]] = {
      val r = summon[RandomValue[Float]]
      Some((0 to 768).map(x => r.generate).toVector)
    }

  given ExecutionContext = ExecutionContext.fromExecutor(Executors.newVirtualThreadPerTaskExecutor())

  val stmt = conn.createStatement()
  val rs = stmt.executeQuery("SELECT tablename, schemaname FROM pg_tables WHERE schemaname = 'public' or tablename = 'schemaname' ")
  while (rs.next()) {
    println(rs.getString("tablename") + " " +  rs.getString("schemaname"))
  }

  // Summon for the constructor `assets`, not an applied type
  val summonAssets = summon[RandomGenerator[New[assets]]]

  val testAssets = summon[DatabaseTest[assets]]
  testAssets.test(conn)

  val testActions = summon[DatabaseTestNoUpdate[actions]]
  testActions.test(conn)
  val testAssetBookmarks = summon[DatabaseTestNoUpdate[asset_bookmarks]]
  testAssetBookmarks.test(conn)

  val testAssetCollection = summon[DatabaseTestNoUpdate[asset_collection]]
  testAssetCollection.test(conn)

  val testAssetProduct = summon[DatabaseTestNoUpdate[asset_product]]
  testAssetProduct.test(conn)

  val testAssetRatings = summon[DatabaseTest[asset_ratings]]
  testAssetRatings.test(conn)

  val testAssetTypes = summon[DatabaseTest[asset_types]]
  testAssetTypes.test(conn)



  val testAttributes = summon[DatabaseTest[attributes]]
  testAttributes.test(conn)

  val testAttributeValues = summon[DatabaseTest[attributevalues]]
  testAttributeValues.test(conn)

  val testBrands = summon[DatabaseTest[brands]]
  testBrands.test(conn)

  val testCollections = summon[DatabaseTest[collections]]
  testCollections.test(conn)

  val testComments = summon[DatabaseTest[comments]]
  testComments.test(conn)


  val testEntities = summon[DatabaseTest[entities]]
  testEntities.test(conn)

  val testEntityAttributes = summon[DatabaseTestNoUpdate[entityattributes]]
  testEntityAttributes.test(conn)

  val testGeos = summon[DatabaseTestNoUpdate[geos]]
  testGeos.test(conn)

  val testMappings = summon[DatabaseTestNoUpdate[mappings]]
  testMappings.test(conn)

  val testNewPractices = summon[DatabaseTestNoUpdate[new_practices]]
  testNewPractices.test(conn)

  val testNominations = summon[DatabaseTest[nominations]]
  testNominations.test(conn)

  val testOfferingTypes = summon[DatabaseTest[offering_types]]
  testOfferingTypes.test(conn)

  val testParent = summon[DatabaseTest[parent]]
  testParent.test(conn)

  val testPractices = summon[DatabaseTest[practices]]
  testPractices.test(conn)

  val testProducts = summon[DatabaseTest[products]]
  testProducts.test(conn)

  val testRelationship = summon[DatabaseTest[relationship]]
  testRelationship.test(conn)


  testAssets.test(conn)

