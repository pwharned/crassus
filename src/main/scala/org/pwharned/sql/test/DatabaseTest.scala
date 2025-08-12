package org.pwharned.sql.test
import org.pwharned.openapi.Schema
import org.pwharned.parse.QueryDeserializer
import org.pwharned.sql.database.Connection.*
import org.pwharned.sql.database.HKD.*
import org.pwharned.sql.database.{DbTypeMapper, FieldBinder, Row}
import org.pwharned.sql.derive.*
import org.pwharned.utils.{RandomGenerator, Randomizer}

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.deriving.Mirror

// G is your HKD case class type constructor, e.g. assets
trait DatabaseTest[P[_[_]]]:
  def test(conn: java.sql.Connection): Unit

object DatabaseTest:

  // We derive for the constructor G (not for an applied type).
  // Mirror for G[Id] (or whatever identity you use) is enough to drive derivations.
  inline given derived[ T[_[_]]<: Product](using
                                // A concrete instantiation to obtain a Mirror-derived structure
                                                row: Row[Persisted[T]],
                                                m: Mirror.ProductOf[Persisted[T]],
                                                qp: QueryDeserializer[Optional[T]],
                                                sch: Schema[Persisted[T]],
                                                sqli: SqlInsert[New[T]],
                                                sqlo:SqlSelect[Optional[T]],
                                                fbo: FieldBinder[Optional[T]],
                                               rg: RandomGenerator[New[T]],
                                               fb: FieldBinder[New[T]],
                                               randomizer: Randomizer[Persisted[T]],
                                               sqlu:SqlUpdate[Persisted[T]],
                                               sqld: SqlDelete[Persisted[T]],
                                               typeMapper: DbTypeMapper
                               
                               ): DatabaseTest[T] =
    new DatabaseTest[T]:
      def test(conn: java.sql.Connection): Unit =
        given ExecutionContext =
          ExecutionContext.fromExecutor(Executors.newVirtualThreadPerTaskExecutor())
        // Schema creation for persisted rows
        conn.createTable[Persisted[T]]

        // Insert → Randomize → Update cycle
        (0 to 5).foreach { _ =>
          val recNew: New[T] = rg.generate

          val recPersisted1: Persisted[T] =
            conn.insert[New[T], Persisted[T]](recNew).next()

          val recPersisted2: Persisted[T] = randomizer.randomize(recPersisted1)

          val pkeys: PrimaryKeyFields[Persisted[T]]#Out =
            TupleKeyExtractor
              .extractPkTuple(recPersisted2)
              .asInstanceOf[PrimaryKeyFields[Persisted[T]]#Out]

          val recPersisted3: Persisted[T] =
            conn.update[Persisted[T], Persisted[T]](recPersisted2, pkeys).next()

          assert(recPersisted3 != recPersisted2)
        }

        // Query → parse primary keys → select → delete
        val stream = conn.query[Persisted[T]]

        stream.foreach { row =>
          val pkeys: PrimaryKeyFields[Persisted[T]]#Out =
            TupleKeyExtractor
              .extractPkTuple(row)
              .asInstanceOf[PrimaryKeyFields[Persisted[T]]#Out]

          val pkeyStrings: Seq[String] =
            pkeys.productIterator.toSeq.collect {
              case pk: PrimaryKey[?] => pk.value.toString
            }

          val parseKeys = PrimaryKeyParser.makeParser[Persisted[T]]
          val keyTuple: PrimaryKeyFields[Persisted[T]]#Out = parseKeys(pkeyStrings)

          val selected = conn.query[Persisted[T]](keyTuple).next()
          val _ = conn.delete[Persisted[T]](keyTuple)
        }
