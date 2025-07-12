package org.pwharned.sql

import org.pwharned.sql.database.HKD.*

// You’ll need the Insertable derived instance and the HKD/User definition in scope:
object InsertableSqlTests:

  // 1) Re-declare your model in this test file:
  case class User[F[_]](
                         id: F[PrimaryKey[Int]],
                         name: F[String],
                         age: F[Default[Int]]
                       )

  // 2) A small helper to compare expected vs. actual
  private def test(name: String, got: String, expected: String): Unit =
    assert(
      got == expected,
      s"\n[$name]\n  expected: $expected\n       got: $got\n"
    )

  @main def runAll(): Unit =
    // CASE A: only `name` is provided
    // New[User]  ≃  User[NewField]
    val uA: New[User] = User[NewField](None, "Bob", None)
    val insA = Insertable[New[User]]
    val sqlA = insA.sql(uA)
    test(
      "only name",
      sqlA,
      "insert into users(name) values(?)"
    )

    // CASE B: id + name + age all provided
    val uB: New[User] = User(
      Some(PrimaryKey(42)),
      "Alice",
      Some(30)
    )
    val sqlB = insA.sql(uB)
    test(
      "id + name + age",
      sqlB,
      "insert into users(id, name, age) values(?, ?, ?)"
    )

    // CASE C: name + age (no id)
    val uC: New[User] = User(
      None,
      "Charlie",
      Some(25)
    )
    val sqlC = insA.sql(uC)
    test(
      "name + age",
      sqlC,
      "insert into users(name, age) values(?, ?)"
    )


    println("All Insertable SQL tests passed!")
