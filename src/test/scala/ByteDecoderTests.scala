import org.pwharned.codec.ByteDecoder

import java.nio.charset.StandardCharsets
import org.pwharned.codec.ByteDecoder.*

case class Address(street: String, city: String) derives ByteDecoder
case class Person(
                   name: String,
                   age: Int,
                   active: Boolean,
                   address: Address,
                   spouse: Option[Person] = None
                 ) derives ByteDecoder

type PersonRow = (name: String, age: Int)
given ByteDecoder[PersonRow] = ByteDecoder.derived

@main def runTests(): Unit = {
  def assertSuccess[T](result: Either[String, T], expected: T): Unit =
    assert(result == Right(expected), s"Expected $expected but got $result")

  def assertFailure[T](result: Either[String, T]): Unit =
    assert(result.isLeft, s"Expected failure but got $result")

  // ─── 1. Basic Person with nested spouse ──────────────────────────────────────
  val json =
    """
      |{
      |  "name": "Alice",
      |  "age": 30,
      |  "active": true,
      |  "address": { "street": "123 Maple St", "city": "Austin" },
      |  "spouse": {
      |    "name": "Bob",
      |    "age": 32,
      |    "active": false,
      |    "address": { "street": "456 Oak Ave", "city": "Dallas" }
      |  }
      |}
    """.stripMargin

  val buf = json.getBytes(StandardCharsets.UTF_8)
  val res = summon[ByteDecoder[Person]].decode(buf, 0, buf.length)
  assertSuccess(res,
    Person("Alice", 30, true, Address("123 Maple St", "Austin"),
      Some(Person("Bob", 32, false, Address("456 Oak Ave", "Dallas"), None)))
  )

  // ─── 2. Person with no spouse ────────────────────────────────────────────────
  val jsonNoSpouse =
    """
      |{
      |  "name": "Charlie",
      |  "age": 40,
      |  "active": true,
      |  "address": { "street": "789 Pine Rd", "city": "Houston" }
      |}
    """.stripMargin

  val bufNoSpouse = jsonNoSpouse.getBytes(StandardCharsets.UTF_8)
  val resNoSpouse = summon[ByteDecoder[Person]].decode(bufNoSpouse, 0, bufNoSpouse.length)
  assertSuccess(resNoSpouse,
    Person("Charlie", 40, true, Address("789 Pine Rd", "Houston"), None)
  )

  // ─── 3. Malformed JSON ───────────────────────────────────────────────────────
  val badJson =
    """{ "name": "Dana", "age": "oops", "active": true }"""
  val bufBad = badJson.getBytes(StandardCharsets.UTF_8)
  val resBad = summon[ByteDecoder[Person]].decode(bufBad, 0, bufBad.length)
  assertFailure(resBad)

  // ─── 4. Missing required field ───────────────────────────────────────────────
  val missingFieldJson =
    """{ "name": "Eve", "active": true }"""
  val bufMissing = missingFieldJson.getBytes(StandardCharsets.UTF_8)
  val resMissing = summon[ByteDecoder[Person]].decode(bufMissing, 0, bufMissing.length)
  assertFailure(resMissing)

  // ─── 5. Tuple decoding ───────────────────────────────────────────────────────
  val personRowJson =
    """{ "name": "Jack", "age": 1 }"""
  val bufRow = personRowJson.getBytes(StandardCharsets.UTF_8)
  val resRow = summon[ByteDecoder[PersonRow]].decode(bufRow, 0, bufRow.length)
  assertSuccess(resRow, ("Jack", 1))

  // ─── 6. Optional field explicitly null ───────────────────────────────────────
  val nullSpouseJson =
    """
      |{
      |  "name": "Karen",
      |  "age": 28,
      |  "active": true,
      |  "address": { "street": "321 Birch Blvd", "city": "San Antonio" },
      |  "spouse": null
      |}
    """.stripMargin

  val bufNullSpouse = nullSpouseJson.getBytes(StandardCharsets.UTF_8)
  val resNullSpouse = summon[ByteDecoder[Person]].decode(bufNullSpouse, 0, bufNullSpouse.length)
  assertSuccess(resNullSpouse,
    Person("Karen", 28, true, Address("321 Birch Blvd", "San Antonio"), None)
  )

  println("✅ All tests passed!")
}
