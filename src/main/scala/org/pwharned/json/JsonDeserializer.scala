package org.pwharned.json

import org.pwharned.database.HKD.{Nullable, PrimaryKey}
import org.pwharned.parse.{Parse, ParseError, Primitives}

import scala.compiletime.*
import scala.deriving.*
import scala.quoted.*
import org.pwharned.database.HKD.~>.idToId

inline def showTypeMacro[T]: String = ${ showTypeMacroImpl[T] }

def showTypeMacroImpl[T: Type](using Quotes): Expr[String] =
  import quotes.reflect._
  Expr(TypeRepr.of[T].show)

trait JsonDeserializer[T]:
  def deserialize(s: String): Either[ParseError, T]

object JsonDeserializer extends Parse:

  trait JsonFieldParser[A]:
    def parser: Parser[A]

  object JsonFieldParser:
    given JsonFieldParser[String] with
      def parser: Parser[String] = Primitives.quotedString

    given JsonFieldParser[Int] with
      def parser: Parser[Int] = Primitives.intParser

    given JsonFieldParser[Boolean] with
      def parser: Parser[Boolean] = Primitives.boolParser
    given JsonFieldParser[Float] with
      def parser: Parser[Float] = Primitives.floatParser
    given JsonFieldParser[java.util.UUID] with
      def parser: Parser[java.util.UUID] = Primitives.quotedString.map(x => java.util.UUID.fromString(x))
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
          Right((Some(null.asInstanceOf[T]), trimmed.drop("null".length)))
        else
          underlying.parser(input) match {
            case Right((value, rest)) => Right((Some(value), rest))
            case Left(err)            => Left(err)
          }

    given listParser[T](using underlying: JsonFieldParser[T]): JsonFieldParser[List[T]] with
      def parser: String => Either[ParseError, (List[T], String)] =
        input =>
          val inner = input.trim
            .stripPrefix("[")
            .stripSuffix("]")
            .trim

          if inner.isEmpty then
            Right((Nil, ""))
          else
            val tokens = inner.split(",").toList.map(_.trim)

            // foldLeft to accumulate (List[T], "") without needing a final reverse:
            val init: Either[ParseError, (List[T], String)] = Right((List.empty, ""))
            val result = tokens.foldLeft(init) { (accE, tok) =>
              for {
                (xs, _) <- accE // current List[T]
                (x, rem) <- underlying.parser(tok) // parse this one
              } yield (xs :+ x, "") // append x onto xs
            }

            result
            
  def keyValuePair[A](key: String, valueParser: Parser[A]): Parser[A] =
    for {
      _     <- char('"')
      _     <- string(key)
      _     <- char('"')
      _     <- char(':')
      _     <- whitespace
      value <-valueParser
      _ <- comma.optional
      _ <- whitespace
    } yield value

  // If a key is missing, return None without consuming input.
  def optKeyValuePair[A](key: String, valueParser: Parser[A]): Parser[Option[A]] =
    input =>
      if input.trim.startsWith("\"" + key + "\"") then
        keyValuePair(key, valueParser)(input).map { case (v, rest) => (Some(v), rest) }
      else
        Right((None, input))

  // Selects the correct parser based on field type.
  inline def fieldParser[h](key: String): Parser[h] = {
    inline erasedValue[h] match {
      case _: Option[t] =>

        optKeyValuePair(key, summonInline[JsonFieldParser[t]].parser).asInstanceOf[Parser[h]]
      case _ =>
        keyValuePair(key, summonInline[JsonFieldParser[h]].parser)
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


