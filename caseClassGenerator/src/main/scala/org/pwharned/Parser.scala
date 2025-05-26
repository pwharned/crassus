package org.pwharned

object Parse {

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

  val columnparser: Parser[Column] =
    for {
      _ <- whitespace
      name <- identifier
      _ <- whitespace
      dtype <- identifier
    } yield Column(name, DataType.fromString(dtype).get, Some(true))


  val columnListParser: Parser[List[Column]] =
    for {
      first <- columnparser
      rest <- (comma.flatMap(_ => columnparser)).many
    } yield first :: rest


  def identifier: Parser[String] = input => {
    val spaces = input.takeWhile( x=> x.isLetterOrDigit || x == '_')
    Right((spaces, input.drop(spaces.length) ))
  }


  sealed trait DataType {
    def sqlName: String
  }
    case object Integer extends DataType{
     val sqlName =  "INTEGER"
    }
    case object String extends DataType {
     val sqlName =  "TEXT"
    }
    case object  Boolean extends DataType{
      val sqlName = "BOOLEAN"
    }
  case object Float extends DataType{
    val sqlName = "FLOAT"
  }
  case object Date extends DataType{
    val sqlName = "DATE"
  }



  object DataType {
    val values: List[DataType] = List(Integer, String, Boolean, Float)
    def fromString(s: String): Option[DataType] = values.find(_.sqlName.equalsIgnoreCase(s))
  }


  case class Table(name: String, columns: List[Column])

  case class Column(name: String, dataType: DataType, nullable: Option[Boolean])

  implicit class ColumnOps(column: Column){
  def toField:  String = s"${column.name} : ${column.dataType} "
  }

  val createTableParser: Parser[Table] =
    for {
      _ <- stringInsensitive("create")
      _ <- whitespace
      _ <- stringInsensitive("table")
      - <- whitespace
      name <- identifier
      - <- char('(')
      columns <- columnListParser
      - <- char(')')
    } yield Table(name, columns)

}