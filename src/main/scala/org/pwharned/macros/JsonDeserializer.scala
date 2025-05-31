package org.pwharned.macros

import generated.PrimaryKey
import org.pwharned.parse.{Parse, ParseError}

import scala.deriving.*
import scala.compiletime.*

/** A JsonDeserializer for a type T returns either a ParseError or an instance of T. */
trait JsonDeserializer[T]:
  def deserialize(s: String): Either[ParseError, T]

object JsonDeserializer extends Parse:

  ////////////////////////////////////////////////////////////
  // 1. Field Parser – choose parser by field type at compile time
  ////////////////////////////////////////////////////////////
  trait JsonFieldParser[A]:
    def parser: Parser[A]

  object JsonFieldParser:
    given JsonFieldParser[String] with
      def parser: Parser[String] =
        for {
          _ <- char('"')
          s <- identifier  // (You might later enhance this to allow internal spaces etc.)
          _ <- char('"')
        } yield s

    given JsonFieldParser[Int] with
      def parser: Parser[Int] = input =>
        val neg = if input.startsWith("-") then "-" else ""
        val inputAfterNeg = if neg.nonEmpty then input.drop(1) else input
        val digits = inputAfterNeg.takeWhile(_.isDigit)
        if digits.isEmpty then Left(ParseError(0, input, "Expected integer"))
        else
          try
            val value = (neg + digits).toInt
            Right((value, inputAfterNeg.drop(digits.length)))
          catch
            case _: Exception => Left(ParseError(0, input, "Invalid integer format"))

    given JsonFieldParser[Boolean] with
      def parser: Parser[Boolean] = input =>
        if input.startsWith("true") then
          Right((true, input.drop("true".length)))
        else if input.startsWith("false") then
          Right((false, input.drop("false".length)))
        else
          Left(ParseError(0, input, "Expected boolean (true/false)"))

  // NEW: A given instance for PrimaryKey[T] if a JsonFieldParser[T] exists.
    given [T](using underlying: JsonFieldParser[T]): JsonFieldParser[PrimaryKey[T]] with
      def parser: Parser[PrimaryKey[T]] =
        underlying.parser.map(PrimaryKey(_))

  given [T](using underlying: JsonFieldParser[T]): JsonFieldParser[Option[T]] with
    def parser: Parser[Option[T]] =
      underlying.parser.map(Option(_))

  def keyValuePair[A](key: String, valueParser: Parser[A]): Parser[A] =
    for {
      _     <- char('"')
      _     <- string(key)
      _     <- char('"')
      _     <- char(':')
      _     <- whitespace
      value <- valueParser
    } yield value

  ////////////////////////////////////////////////////////////
  // 3. Inline recursion to derive a parser for a tuple of field values
  ////////////////////////////////////////////////////////////
  inline def deriveParsers[T <: Tuple](fieldNames: List[String]): Parser[T] =
    inline erasedValue[T] match
      case _: EmptyTuple => input => Right((EmptyTuple.asInstanceOf[T], input))
      case _: (h *: t) =>
        val key = fieldNames.head
        val headParser: Parser[h] = keyValuePair(key, summonInline[JsonFieldParser[h]].parser)
        val tailNames = fieldNames.tail
        if tailNames.isEmpty then
          input =>
            headParser(input) match
              case Left(err)          => Left(err)
              case Right((hValue, r)) =>
                // Cons the head value with EmptyTuple and ascribe to T
                Right((hValue *: EmptyTuple).asInstanceOf[T], r)
        else
          input =>
            headParser(input) match
              case Left(err) => Left(err)
              case Right((hValue, r)) =>
                comma(r) match
                  case Left(err) => Left(err)
                  case Right((_, rAfterComma)) =>
                    deriveParsers[t](tailNames)(rAfterComma) match
                      case Left(err)                    => Left(err)
                      case Right((tValues, rFinal)) =>
                        // Combine head with tail and cast the result as T
                        Right((hValue *: tValues).asInstanceOf[T], rFinal)

  ////////////////////////////////////////////////////////////
  // 4. Derived JsonDeserializer: generate a parser for any case class
  ////////////////////////////////////////////////////////////
  inline given derived[T <: Product](using m: Mirror.ProductOf[T]): JsonDeserializer[T] =
    new JsonDeserializer[T]:
      def deserialize(s: String): Either[ParseError, T] =
        // Extract the field names from the case class at compile time
        val fieldNames: List[String] =
          constValueTuple[m.MirroredElemLabels].toIArray.toList.map(_.toString)
        // Build a parser that expects:
        //   { <whitespace> field1, comma, field2, ... , lastField <whitespace> }
        val parser: Parser[m.MirroredElemTypes] =
          for {
            _ <- whitespace
            _      <- char('{')
            _      <- whitespace
            values <- deriveParsers[m.MirroredElemTypes](fieldNames)
            _      <- whitespace
            _      <- char('}')
          } yield values

        parser(s) match
          case Right((values, remaining)) =>
            if remaining.trim.nonEmpty then
              Left(ParseError(0, remaining, "Extra input after JSON"))
            else
              try Right(m.fromTuple(values))
              catch case e: Exception =>
                Left(ParseError(0, s, s"Error constructing instance: ${e.getMessage}"))
          case Left(err) => Left(err)
