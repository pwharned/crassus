
package org.pwharned
import org.pwharned.parse.ParseBuffer
import org.pwharned.parse.ParseBuffer.{flatMap, map}
import java.nio.ByteBuffer
@main
def testParse(): Unit =
  
  val input1 = "ab"
  val buffer1 = ByteBuffer.wrap(input1.getBytes("UTF-8"))
  val parser = for{
    a <- ParseBuffer.char('a'.toByte)
    b <- ParseBuffer.char('b'.toByte)
  } yield (a, b)
  val result1 = parser(buffer1)
  result1 match
    case Right((parsedChar, remaining)) =>
      println(s"Test 1 Passed: Parsed char [$parsedChar] with remaining info: [$remaining]")
    case Left(error) =>
      println(s"Test 1 Failed: Error -> $error")


  import org.pwharned.database.HKD.*
  import org.pwharned.database.HKD.Conversions.given 
  import org.pwharned.json.deserialize
  case class User[F[_]](
                         id: F[PrimaryKey[Int]],
                         name: F[Nullable[String]], test: F[String]
                       )

  val a: New[User] = User(None, "Alice", "Hello")
  val b: Updated[User] = User(None, "Alice", "Hello")
  val c: Persisted[User] = User(1, "Alice", "Hello")



  val newUserWithMissingRequiredField = """
                         |{"name":"test"}
                         |""".stripMargin.deserialize[New[User]]
  newUserWithMissingRequiredField match {
    case Left(value) => println("Succesfully failed parsing required fields")
    case Right(value) => throw Exception("Parsing should fail since test is a not nullable field")
  }

  val newUserWithAllField = """
                                          |{"name":"test""test":"test"}
                                          |""".stripMargin.deserialize[New[User]]
  newUserWithAllField match {
    case Left(value) => throw Exception(value.message)
    case Right(value) => println("Succeeded in correctly parsing a new user with all fields but the primary key ")
  }
  
  val updatedUser =
    """
      |{"name":"newName"}
      |""".stripMargin.deserialize[Updated[User]]

  updatedUser match {
    case Left(value) => throw Exception(value.message)
    case Right(value) => println("Successful parse: an updated user has all fields set to optional")
  }
  
  val persistedUser =
    """{"name":"test"}""".stripMargin.deserialize[Persisted[User]]
    
  persistedUser match {
    case Left(value) => println("Fail is correct, a Persisted value cannot be missing a Primary Key or non Nullable field")
    case Right(value) => throw Exception("This should faila s the persisted user is incomplete and cannot be parsed")
  }
  
  val completeUser =
    """
    {"id":1,"name":"myname","test":"test"}
      |""".stripMargin.deserialize[Persisted[User]]
  completeUser match {
    case Left(value) => throw Exception("This should not have failed, user is complete " + value.message + " " + value.input)
    case Right(value) => println("Succesful parsing")
  }
