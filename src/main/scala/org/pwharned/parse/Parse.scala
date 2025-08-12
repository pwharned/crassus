package org.pwharned.parse

import java.nio.ByteBuffer
import scala.annotation.tailrec
import scala.deriving.*
import scala.compiletime.*

case class ParseError(position: Int, input: String, message: String):
  def merge(other: ParseError): ParseError =
    val bestPos = this.position max other.position
    val combined = (this.message ++ other.message).distinct
    ParseError(bestPos,input, combined)
type Parser[T] = String => Either[ParseError, (T, String)]

trait Parse:

  extension [A](p: Parser[A])
    /** parse p, then q, but return p’s result */
    inline def <*[B](q: Parser[B]): Parser[A] = input =>
      p(input).flatMap { case (a, rest1) =>
        q(rest1).map { case (_, rest2) =>
          (a, rest2)
        }
      }

    def sepBy(sep: Parser[Any]): Parser[List[A]] =
      input =>
        // First, try to parse one element
        p(input) match {
          // If we fail right away, return empty list without consuming input
          case Left(_) => Right((Nil, input))

          // We got a head, now loop for more
          case Right((head, rest0)) =>
            @tailrec
            def loop(acc: List[A], in: String): Either[ParseError, (List[A], String)] =
              sep(in) match {
                // no separator ⇒ we're done
                case Left(_) =>
                  Right((acc.reverse, in))

                // consumed sep; now try another p
                case Right((_, afterSep)) =>
                  p(afterSep) match {
                    // if parsing the next element fails, we stop
                    case Left(_) =>
                      Right((acc.reverse, afterSep))

                    // got another element; keep going
                    case Right((next, rest2)) =>
                      loop(next :: acc, rest2)
                  }
              }

            // start the loop with the first element
            loop(List(head), rest0)
        }
    def flatMap[B](f: A => Parser[B]): Parser[B] = input =>
      p(input).flatMap { case (value, rest) => f(value)(rest) }

    def map[B](f: A => B): Parser[B] = input =>
      p(input).map { case (value, rest) => (f(value), rest) }

    def many: Parser[List[A]] = input =>
      p(input) match
        case Right((value, rest)) =>
          many(rest).map { case (values, remaining) => (value :: values, remaining) }
        case Left(_) => Right((Nil, input))


    def optional: Parser[Option[A]] = input =>
      p(input) match
        case Right((value, rest)) => Right((Some(value), rest))
        case Left(_) => Right((None, input))

    def alt(pAlt: Parser[A]): Parser[A] = input =>
      p(input).orElse(pAlt(input))

    def or[B >: A](other: Parser[B]): Parser[B] = new Parser[B]:
      override def apply(input: String): Either[ParseError, (B, String)] =
        // 1. try the first parser
        this (input) match
          case right@Right(_) =>
            right
  
          case Left(err1) =>
            // 2. on failure, backtrack: feed the original input to `other`
            other(input) match
              case right2@Right(_) =>
                right2
  
              case Left(err2) =>
                // 3. both failed: combine errors (see below)
                Left(err1.merge(err2))
    
    def token: Parser[A] =
      for {
        _ <- whitespace
        a <- p
        _ <- whitespace
      } yield a

  def char(c: Char): Parser[Char] = input =>
    input.headOption match
      case Some(value) if value == c => Right((value, input.tail))
      case Some(value) => Left(ParseError(0, input, s"Expected '$c', found '$value'"))
      case None => Left(ParseError(0, input, s"Unexpected end of input, expected '$c'"))


  def string(s: String): Parser[String] = input =>
    if input.startsWith(s) then Right((s, input.drop(s.length)))
    else Left(ParseError(0, input, s"Expected '$s'"))

  def stringInsensitive(s: String): Parser[String] = input =>
    if input.toLowerCase.startsWith(s.toLowerCase) then Right((s, input.drop(s.length)))
    else Left(ParseError(0, input, s"Expected '$s'"))

  def whitespace: Parser[String] = input =>
    val spaces = input.takeWhile( x=> x.isWhitespace || x == '\n')
    Right((spaces, input.drop(spaces.length)))

  def comma: Parser[Unit] = input =>
    if input.startsWith(",") then Right(((), input.drop(1).dropWhile(_.isWhitespace)))
    else Left(ParseError(0, input, "Expected ',' separator"))

  def identifier: Parser[String] = input =>
    val id = input.takeWhile(c => c.isLetterOrDigit || c == '_')
    Right((id, input.drop(id.length)))

  def stringInline: Parser[String] = input =>
    val id = input.takeWhile(c => c!= '"')
    Right((id, input.drop(id.length)))



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
  def stringNoAmpersand: Parser[String] = input =>
    val id = input.takeWhile(c => c!= '&')
    Right((id, input.drop(id.length)))

  def nullParser[T]: Parser[Option[T]] =
    for {
      s <- string("null")
    } yield Some(null).asInstanceOf[Option[T]]

  def intParser: Parser[Int] =
    input =>
      val neg = if input.startsWith("-") then "-" else ""
      val inputAfterNeg = if neg.nonEmpty then input.drop(1) else input
      val digits = inputAfterNeg.takeWhile( x => x.isDigit || x =='.')
      if digits.isEmpty || digits.contains('.') then Left(ParseError(0, input, s"Expected integer, found $input"))
      else
        try {
          val value = (neg + digits).toInt
          Right((value, inputAfterNeg.drop(digits.length)))
        } catch {
          case _: Exception => Left(ParseError(0, input, "Invalid integer format"))
        }
  def longParser: Parser[Long] =
    input =>
      val neg = if input.startsWith("-") then "-" else ""
      val inputAfterNeg = if neg.nonEmpty then input.drop(1) else input
      val digits = inputAfterNeg.takeWhile( x => x.isDigit || x =='.')
      if digits.isEmpty || digits.contains('.') then Left(ParseError(0, input, s"Expected Long, found $input"))
      else
        try {
          val value = (neg + digits).toInt
          Right((value, inputAfterNeg.drop(digits.length)))
        } catch {
          case _: Exception => Left(ParseError(0, input, "Invalid integer format"))
        }


  def numberToken(input: String): Either[ParseError, (String, String)] = {
    // Regex for full decimal with optional exponent
    val Num = """[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?""".r

    Num.findPrefixOf(input) match {
      case Some(tok) => Right((tok, input.substring(tok.length)))
      case None => Left(ParseError(0, input, s"Expected number, found $input"))
    }
  }
  def boolParser: Parser[Boolean] =
    input =>
      if input.startsWith("true") then Right((true, input.drop("true".length)))
      else if input.startsWith("false") then Right((false, input.drop("false".length)))
      else Left(ParseError(0, input, s"Expected boolean, found $input"))

  def doubleParser: Parser[Double] = input =>
    numberToken(input).flatMap { case (tok, rest) =>
      try Right((tok.toDouble, rest))
      catch {
        case _: Exception =>
          Left(ParseError(0, input, s"Invalid double format: $tok"))
      }
    }

  def floatParser: Parser[Float] = input =>
    numberToken(input).flatMap { case (tok, rest) =>
      try Right((tok.toFloat, rest))
      catch {
        case _: Exception =>
          Left(ParseError(0, input, s"Invalid float format: $tok"))
      }
    }