package org.pwharned

trait Parse {

  case class ParseError(position: Int, input: String, message: String)

  type Parser[T] = String => Either[ParseError, (T, String)]

  def char(c: Char): Parser[Char] = input => input.toList.headOption match {
    case Some(value) if value == c => Right((value, input.tail))
    case Some(value) => Left(ParseError(0, input, s"Expected '$c', found '$value'"))
    case _ => Left(ParseError(0, input, s"Unexpected end of input , expected '$c''"))
  }
  implicit class ParserOps [A](p: Parser[A]) {
    def flatMap[B](f: A => Parser[B]): Parser[B] = input => p(input).flatMap( value => f(value._1)(value._2))
    def map[B](f: A => B): Parser[B] = input => p(input).map { case (value, rest) => (f(value), rest) }
    def many: Parser[List[A]] = input => p(input) match {
      case Right((value, rest)) => many(rest).map {
        case (values, remaining) => (value :: values, remaining)
      }
      case Left(_) => Right((Nil, input))
    }

    def optional: Parser[Option[A]] = input => p(input) match {
      case Right((value, rest)) => Right(Some(value), rest)
      case Left(rest) => Right((None, rest.input))
    }
    def or(p: Parser[A]): Parser[A] = input => p(input) match {
      case Right(rest) => Right(rest)
      case Left(rest) => p(input)
    }
  }


  def string(s: String): Parser[String] = input => if (input.startsWith(s)) Right((s, input.drop(s.length))) else Left(ParseError(0, input, s"Expected '$s''"))

  def stringInsensitive(s: String): Parser[String] = input => if (input.toLowerCase.startsWith(s.toLowerCase)) Right((s, input.drop(s.length))) else Left(ParseError(0, input, s"Expected '$s''"))

  def whitespace: Parser[String] = input => {
    val spaces = input.takeWhile(_.isWhitespace)
    Right((spaces, input.drop(spaces.length) ))
  }


  def comma: Parser[Unit] = input => input match {
    case s if s.startsWith(",") => Right(((), s.drop(1).dropWhile(_.isWhitespace)))
    case _ => Left(ParseError(0, input, "Expected ',' separator"))
  }



  def identifier: Parser[String] = input => {
    val spaces = input.takeWhile( x=> x.isLetterOrDigit || x == '_')
    Right((spaces, input.drop(spaces.length) ))
  }







}