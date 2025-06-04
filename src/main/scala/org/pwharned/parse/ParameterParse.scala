package org.pwharned.parse

import org.pwharned.database.HKD.{Nullable, PrimaryKey}
import org.pwharned.parse.{Parse, ParseError, Primitives}

import scala.compiletime.*
import scala.deriving.*
import scala.quoted.*


trait QueryDeserializer[T]:
  def deserialize(s: String): Either[ParseError, T]

object QueryDeserializer extends Parse:

  trait QueryFieldDeserializer[A]:
    def parser: Parser[A]

  object QueryFieldDeserializer:
    given QueryFieldDeserializer[String] with
      def parser: Parser[String] = Primitives.stringNoAmpersand

    given QueryFieldDeserializer[Int] with
      def parser: Parser[Int] = Primitives.intParser

    given QueryFieldDeserializer[Boolean] with
      def parser: Parser[Boolean] = Primitives.boolParser

    // Wrap a parsed T into PrimaryKey[T]
    given [T](using underlying: QueryFieldDeserializer[T]): QueryFieldDeserializer[PrimaryKey[T]] with
      def parser: Parser[PrimaryKey[T]] =
        underlying.parser.map(PrimaryKey(_))
    given [T](using underlying: QueryFieldDeserializer[T]): QueryFieldDeserializer[Nullable[T]] with
      def parser: Parser[Nullable[T]] =
        underlying.parser.map(Nullable(_))

    // For Option[T], first check for "null"; otherwise delegate.
    given [T](using underlying: QueryFieldDeserializer[T]): QueryFieldDeserializer[Option[T]] with
      def parser: Parser[Option[T]] = input =>
        val trimmed = input.trim
          underlying.parser(input) match {
            case Right((value, rest)) => Right((Some(value), rest))
            case Left(err)            => Left(err)
          }

  // A helper that parses a key/value pair.
  def keyValuePair[A](key: String, valueParser: Parser[A]): Parser[A] =
    for {
      _     <- string(key)
      _     <- char('=')
      value <-valueParser
      _ <- char('&').optional
    } yield value

  // If a key is missing, return None without consuming input.
  def optKeyValuePair[A](key: String, valueParser: Parser[A]): Parser[Option[A]] =
    input =>
      // Check if the input starts with the expected key followed by '='.
      if (input.trim.startsWith(key + "=")) {
        // Parse the key-value pair and wrap the result in Some.
        keyValuePair(key, valueParser)(input).map { case (v, rest) => (Some(v), rest) }
      } else {
        // The key is not present, so return None and don't consume input.
        Right((None, input))
      }

  // Selects the correct parser based on field type.
  inline def fieldParser[h](key: String): Parser[h] = {
    inline erasedValue[h] match {
      case _: Option[t] =>

        optKeyValuePair(key, summonInline[QueryFieldDeserializer[t]].parser).asInstanceOf[Parser[h]]
      case _ =>
        keyValuePair(key, summonInline[QueryFieldDeserializer[h]].parser)
    }
  }


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
            case Left(err) =>Left(err)
            case Right((hValue, r)) =>             deriveParsers[t](tailNames)(r) match {
              case Left(err) => Left(err)
              case Right((tValues, rFinal)) => Right((hValue *: tValues).asInstanceOf[T], rFinal)
            }

          }

  inline given derived[T <: Product](using m: Mirror.ProductOf[T]): QueryDeserializer[T] =
    new QueryDeserializer[T]:
      def deserialize(s: String): Either[ParseError, T] =
        // Get names from the case class at compile time.
        val fieldNames: List[String] =
          constValueTuple[m.MirroredElemLabels].toIArray.toList.map(_.toString)
        // Build a JSON object parser:
        val parser: Parser[m.MirroredElemTypes] =
          for {
            _      <- char('?').optional
            values <- deriveParsers[m.MirroredElemTypes](fieldNames)
          } yield values

        parser(s) match {
          case Right((tuple, remaining)) =>
            if remaining.trim.nonEmpty then
              Left(ParseError(0, remaining, "Extra input in Query String"))
            else
              try Right(m.fromTuple(tuple))
              catch case e: Exception =>
                Left(ParseError(0, s, s"Error constructing instance: ${e.getMessage}"))
          case Left(err) => Left(err)
        }

extension (s: String)
  def fromQuery[A <: Product](using j: QueryDeserializer[A]): Either[ParseError, A] = summon[QueryDeserializer[A]].deserialize(s)


