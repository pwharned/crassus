package org.pwharned.parse

import java.nio.ByteBuffer
import scala.deriving.*
import scala.compiletime.*

case class ParseError(position: Int, input: String, message: String):
  def merge(other: ParseError): ParseError =
    val bestPos = this.position max other.position
    val combined = (this.message ++ other.message).distinct
    ParseError(bestPos,input, combined)
type Parser[T] = String => Either[ParseError, (T, String)]

trait Parse:


  inline def char(c: Char): Parser[Char] = input =>
    input.headOption match
      case Some(value) if value == c => Right((value, input.tail))
      case Some(value) => Left(ParseError(0, input, s"Expected '$c', found '$value'"))
      case None => Left(ParseError(0, input, s"Unexpected end of input, expected '$c'"))

  extension [A](p: Parser[A])
    inline def flatMap[B](f: A => Parser[B]): Parser[B] = input =>
      p(input).flatMap { case (value, rest) => f(value)(rest) }

    inline def map[B](f: A => B): Parser[B] = input =>
      p(input).map { case (value, rest) => (f(value), rest) }

    def many: Parser[List[A]] = input =>
      p(input) match
        case Right((value, rest)) =>
          many(rest).map { case (values, remaining) => (value :: values, remaining) }
        case Left(_) => Right((Nil, input))
        

    inline def optional: Parser[Option[A]] = input =>
      p(input) match
        case Right((value, rest)) => Right((Some(value), rest))
        case Left(_) => Right((None, input))

    inline def alt(pAlt: Parser[A]): Parser[A] = input =>
      p(input).orElse(pAlt(input))
      
    def or[B >: A](other: Parser[B]): Parser[B] = new Parser[B]:
      override def apply(input: String): Either[ParseError,(B,String)] =
        // 1. try the first parser
        this(input) match
          case right @ Right(_) =>
            right
  
          case Left(err1) =>
            // 2. on failure, backtrack: feed the original input to `other`
            other(input) match
              case right2 @ Right(_) =>
                right2
  
              case Left(err2) =>
                // 3. both failed: combine errors (see below)
                Left(err1.merge(err2))
  inline def string(s: String): Parser[String] = input =>
    if input.startsWith(s) then Right((s, input.drop(s.length)))
    else Left(ParseError(0, input, s"Expected '$s'"))

  inline def stringInsensitive(s: String): Parser[String] = input =>
    if input.toLowerCase.startsWith(s.toLowerCase) then Right((s, input.drop(s.length)))
    else Left(ParseError(0, input, s"Expected '$s'"))

  inline def whitespace: Parser[String] = input =>
    val spaces = input.takeWhile(_.isWhitespace)
    Right((spaces, input.drop(spaces.length)))

  inline def comma: Parser[Unit] = input =>
    if input.startsWith(",") then Right(((), input.drop(1).dropWhile(_.isWhitespace)))
    else Left(ParseError(0, input, "Expected ',' separator"))

  def identifier: Parser[String] = input =>
    val id = input.takeWhile(c => c.isLetterOrDigit || c == '_')
    Right((id, input.drop(id.length)))

  def stringInline: Parser[String] = input =>
    val id = input.takeWhile(c => c!= '"')
    Right((id, input.drop(id.length)))

  def token[A](p: Parser[A]): Parser[A] =
    for {
      _ <- whitespace
      a <- p
      _ <- whitespace
    } yield a


trait ParseBuffer:


  type Parser[T] = ByteBuffer => Either[ParseError, (T, ByteBuffer)]

  inline def char(inline c: Byte): Parser[Char] = buffer =>
    if (buffer.remaining() > 0) {
      val pos = buffer.position()
      val b = buffer.get(pos)
      if (b == c) {
        // advance by one byte
        buffer.position(pos + 1)
        Right((c.toChar, buffer))
      } else {
        Left(ParseError(pos,buffer.get().toString, s"Expected '${c.toChar}', found '${b.toChar}'"))
      }
    } else {
      Left(ParseError(buffer.position(), buffer.get().toString, "Unexpected end of input"))
    }
  extension [A](p: Parser[A])
    inline def flatMap[B](f: A => Parser[B]): Parser[B] = input =>
      p(input).flatMap { case (value, rest) => f(value)(rest) }

    inline def map[B](f: A => B): Parser[B] = input =>
      p(input).map { case (value, rest) => (f(value), rest) }


object ParseBuffer extends  ParseBuffer


// ──────────────────────────────────────────────
object Primitives extends Parse:
  inline def quotedString: Parser[String] =
    for {
      _ <- char('"')
      s <- stringInline
      _ <- char('"')
    } yield s
  inline def stringNoAmpersand: Parser[String] = input =>
    val id = input.takeWhile(c => c!= '&')
    Right((id, input.drop(id.length)))

  inline def nullParser[T]: Parser[Option[T]] =
    for {
      s <- string("null")
    } yield Some(null).asInstanceOf[Option[T]]

  inline def intParser: Parser[Int] =
    input =>
      val neg = if input.startsWith("-") then "-" else ""
      val inputAfterNeg = if neg.nonEmpty then input.drop(1) else input
      val digits = inputAfterNeg.takeWhile(_.isDigit)
      if digits.isEmpty then Left(ParseError(0, input, s"Expected integer, found $input"))
      else
        try {
          val value = (neg + digits).toInt
          Right((value, inputAfterNeg.drop(digits.length)))
        } catch {
          case _: Exception => Left(ParseError(0, input, "Invalid integer format"))
        }

  inline def boolParser: Parser[Boolean] =
    input =>
      if input.startsWith("true") then Right((true, input.drop("true".length)))
      else if input.startsWith("false") then Right((false, input.drop("false".length)))
      else Left(ParseError(0, input, "Expected boolean"))

  inline def floatParser: Parser[Float] =
    input =>
      if input.head.isDigit then {
        val digit = input.takeWhile(x => x.isDigit || x =='.')
        val remaining = input.drop(digit.length)
        Right((digit.toFloat, input.drop("true".length)))
      }
      else Left(ParseError(0, input, "Expected boolean"))
      
      