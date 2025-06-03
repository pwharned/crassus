package org.pwharned.json

import org.pwharned.database.HKD.{PrimaryKey, Nullable}
import org.pwharned.parse.{Parse, ParseError}

import scala.compiletime.*
import scala.deriving.*

trait JsonDeserializer[T]:
  def deserialize(s: String): Either[ParseError, T]

object JsonDeserializer extends Parse:

  // ──────────────────────────────────────────────
  // Module: Primitives
  // ──────────────────────────────────────────────
  object Primitives:
    inline def quotedString: Parser[String] =
      for {
        _ <- char('"')
        s <- stringInline
        _ <- char('"')
      } yield s

    inline def intParser: Parser[Int] =
      input =>
        val neg = if input.startsWith("-") then "-" else ""
        val inputAfterNeg = if neg.nonEmpty then input.drop(1) else input
        val digits = inputAfterNeg.takeWhile(_.isDigit)
        if digits.isEmpty then Left(ParseError(0, input, "Expected integer"))
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

  // ──────────────────────────────────────────────
  // Module: Field Parsers
  // ──────────────────────────────────────────────
  trait JsonFieldParser[A]:
    def parser: Parser[A]

  object JsonFieldParser:
    given JsonFieldParser[String] with
      def parser: Parser[String] = Primitives.quotedString

    given JsonFieldParser[Int] with
      def parser: Parser[Int] = Primitives.intParser

    given JsonFieldParser[Boolean] with
      def parser: Parser[Boolean] = Primitives.boolParser

    // Wrap a parsed T into PrimaryKey[T]
    given [T](using underlying: JsonFieldParser[T]): JsonFieldParser[PrimaryKey[T]] with
      def parser: Parser[PrimaryKey[T]] =
        underlying.parser.map(PrimaryKey(_))
    given [T](using underlying: JsonFieldParser[T]): JsonFieldParser[Nullable[T]] with
      def parser: Parser[Nullable[T]] =
        underlying.parser.map(Nullable(_))

    // For Option[T], first check for "null"; otherwise delegate.
    given [T](using underlying: JsonFieldParser[T]): JsonFieldParser[Option[T]] with
      def parser: Parser[Option[T]] = input =>
        val trimmed = input.trim
        if trimmed.startsWith("null") then
          Right((None, trimmed.drop("null".length)))
        else
          underlying.parser(input) match {
            case Right((value, rest)) => Right((Some(value), rest))
            case Left(err)            => Left(err)
          }

  // A helper that parses a key/value pair.
  def keyValuePair[A](key: String, valueParser: Parser[A]): Parser[A] =
    for {
      _     <- char('"')
      _     <- string(key)
      _     <- char('"')
      _     <- char(':')
      _     <- whitespace
      value <- valueParser
    } yield value

  // If a key is missing, return None without consuming input.
  def optKeyValuePair[A](key: String, valueParser: Parser[A]): Parser[Option[A]] =
    input =>
      if input.trim.startsWith("\"" + key + "\"") then
        keyValuePair(key, valueParser)(input).map { case (v, rest) => (Some(v), rest) }
      else
        Right((None, input))

  // Selects the correct parser based on field type.
  inline def fieldParser[h](key: String): Parser[h] =
    inline erasedValue[h] match {
      case _: Option[t] =>
        optKeyValuePair(key, summonInline[JsonFieldParser[t]].parser).asInstanceOf[Parser[h]]
      case _ =>
        keyValuePair(key, summonInline[JsonFieldParser[h]].parser)
    }

  // ──────────────────────────────────────────────
  // Module: Case-Class Derivation via Tuple Recursion
  // ──────────────────────────────────────────────
  inline def deriveParsers[T <: Tuple](fieldNames: List[String]): Parser[T] =
    inline erasedValue[T] match
      case _: EmptyTuple =>
        input => Right((EmptyTuple.asInstanceOf[T], input))
      case _: (h *: t) =>
        val key = fieldNames.head
        val headParser: Parser[h] = fieldParser[h](key)
        val tailNames = fieldNames.tail
        input =>
          headParser(input) match {
            case Left(err) => Left(err)
            case Right((hValue, r)) =>
              // Conditionally consume comma:
              val nextInputEither: Either[ParseError, String] =
                hValue match {
                  // If the field is optional and missing, do NOT try to match a comma.
                  //case opt: Option[?] if opt.isEmpty => Right(r)
                  case None => Right(r)
                  case _ =>
                    if tailNames.isEmpty then Right(r)
                    else comma(r).map { case (_, afterComma) => afterComma }
                }
              nextInputEither.flatMap { rAfter =>
                deriveParsers[t](tailNames)(rAfter) match {
                  case Left(err) => Left(err)
                  case Right((tValues, rFinal)) =>
                    Right((hValue *: tValues).asInstanceOf[T], rFinal)
                }
              }
          }

  // ──────────────────────────────────────────────
  // Deriving the case-class deserializer
  // ──────────────────────────────────────────────
  inline given derived[T <: Product](using m: Mirror.ProductOf[T]): JsonDeserializer[T] =
    new JsonDeserializer[T]:
      def deserialize(s: String): Either[ParseError, T] =
        // Get names from the case class at compile time.
        val fieldNames: List[String] =
          constValueTuple[m.MirroredElemLabels].toIArray.toList.map(_.toString)
        // Build a JSON object parser:
        val parser: Parser[m.MirroredElemTypes] =
          for {
            _      <- whitespace
            _      <- char('{')
            _      <- whitespace
            values <- deriveParsers[m.MirroredElemTypes](fieldNames)
            _      <- whitespace
            _      <- char('}')
          } yield values

        parser(s) match {
          case Right((tuple, remaining)) =>
            if remaining.trim.nonEmpty then
              Left(ParseError(0, remaining, "Extra input after JSON"))
            else
              try Right(m.fromTuple(tuple))
              catch case e: Exception =>
                Left(ParseError(0, s, s"Error constructing instance: ${e.getMessage}"))
          case Left(err) => Left(err)
        }
        
extension (s: String)
  def deserialize[A <: Product](using j: JsonDeserializer[A]): Either[ParseError, A] = summon[JsonDeserializer[A]].deserialize(s)


