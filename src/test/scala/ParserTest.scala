
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