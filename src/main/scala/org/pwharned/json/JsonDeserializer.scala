package org.pwharned.json

import org.pwharned.database.HKD.{Nullable, PrimaryKey}
import org.pwharned.parse.{Parse, ParseError, Parser, Primitives}

import scala.compiletime.*
import scala.deriving.*
import scala.quoted.*
import org.pwharned.database.HKD.~>.idToId

inline def showTypeMacro[T]: String = ${ showTypeMacroImpl[T] }

def showTypeMacroImpl[T: Type](using Quotes): Expr[String] =
  import quotes.reflect._
  Expr(TypeRepr.of[T].show)

trait JsonDeserializer[T]:
  def deserialize: Parser[T]

object JsonDeserializer extends Parse:

  trait JsonFieldParser[A]:
    def parser: Parser[A]

  object JsonFieldParser:
    given JsonFieldParser[String] with
      def parser: Parser[String] = Primitives.quotedString

    given JsonFieldParser[Int] with
      def parser: Parser[Int] = Primitives.intParser

    given JsonFieldParser[Double] with
      def parser: Parser[Double] = Primitives.doubleParser
    given JsonFieldParser[Long] with
      def parser: Parser[Long] = Primitives.longParser
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


    given jsPrimitiveParser: JsonFieldParser[JsPrimitive] with
      def parser: Parser[JsPrimitive] =
        summon[JsonFieldParser[String]].parser
          .alt(summon[JsonFieldParser[Int]].parser)
          .alt(summon[JsonFieldParser[Boolean]].parser)
          .alt(summon[JsonFieldParser[Long]].parser)
          .alt(summon[JsonFieldParser[Double]].parser)
          .alt(summon[JsonFieldParser[Float]].parser)

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
      def parser: Parser[List[T]] =
        for {
          _ <- char('[') // consume “[”
          head <- underlying.parser // parse first element
          tail <- (comma.flatMap( _ => underlying.parser)).many
          _ <- char(']') // consume “]”
          _ <- whitespace
        } yield head :: tail

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



  def keyValuePair[A](valueParser: Parser[A]): Parser[A] =
        for {
          _ <- char('"')
          key <- stringInline
          _ <- char('"')
          _ <- char(':')
          _ <- whitespace
          value <- valueParser
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
      case _: Product =>keyValuePair(key, summonInline[JsonDeserializer[h]].deserialize)

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

  type M = Map[String, String | Int]

  inline given JsonFieldParser[M] = new JsonFieldParser[M] {
    def parser: Parser[M] = summonInline[JsonDeserializer[M]].deserialize
  }

  inline given jsonFieldParserFromDeserializer[A](using jd: JsonDeserializer[A]): JsonFieldParser[A] =
    new JsonFieldParser[A] {
      def parser: Parser[A] = new Parser[A] {
        def apply(input: String): Either[ParseError, (A, String)] =
          jd.deserialize(input) // no remaining input
      }
    }

  type JsPrimitive = String | Int | Boolean | Long | Null | Double | Float

  enum JsonAst:
    case Obj(fields: Map[String, JsonAst])
    case Arr(items: List[JsonAst])
    case JsValue(value: JsPrimitive)

  type JsonValue[J <: JsonAst] = J match
    case JsonAst.Obj => Map[String, JsonValue[JsonAst]]
    case JsonAst.Arr => List[JsonValue[JsonAst]]
    case JsonAst.JsValue => JsPrimitive

  type ToJson[T] <: JsonAst = T match
    case JsPrimitive => JsonAst.JsValue
    case List[x] => JsonAst.Arr
    case Map[String, x] => JsonAst.Obj

  val sample: JsonAst =
    JsonAst.Obj(Map(
      "name" -> JsonAst.JsValue( "alice"),
      "age" -> JsonAst.JsValue(30.0),
      "tags" -> JsonAst.Arr(List(JsonAst.JsValue("scala"), JsonAst.JsValue("json")))
    ))


  inline given jsonAstDeserializer: JsonDeserializer[JsonAst] =
    new JsonDeserializer[JsonAst] {
      override def deserialize: Parser[JsonAst] =
        new Parser[JsonAst] {
          def apply(input: String): Either[ParseError, (JsonAst, String)] = {
            // 1) primitive → JsonAst.JsValue
            val primP: Parser[JsonAst] =
              summon[JsonFieldParser[JsPrimitive]].parser
                .map(JsonAst.JsValue)

            // 2) array → JsonAst.Arr
            lazy val arrP: Parser[JsonAst] =
              for {
                _ <- char('[') <* whitespace
                xs <- deserialize.sepBy(comma)
                _ <- char(']') <* whitespace
              } yield JsonAst.Arr(xs.toList)

            // 3) object → JsonAst.Obj
            lazy val objP: Parser[JsonAst] =
              for {
                _ <- char('{') <* whitespace
                pairs <- (for {
                  _ <- char('"')
                  key <- stringInline
                  _ <- char('"') <* whitespace
                  _ <- char(':') <* whitespace
                  v <- deserialize
                } yield key -> v).sepBy(comma)
                _ <- char('}') <* whitespace
              } yield JsonAst.Obj(pairs.toMap)

            // try object first, then array, then primitive
            objP(input).orElse(arrP(input)).orElse(primP(input))
          }
        }
    }


  inline given derived[T <: Product](using m: Mirror.ProductOf[T]): JsonDeserializer[T] =
    new JsonDeserializer[T]:
      def deserialize: Parser[T] =
        val fieldNames: List[String] =
          constValueTuple[m.MirroredElemLabels].toIArray.toList.map(_.toString)

        val parser: Parser[m.MirroredElemTypes] =
          for {
            _ <- whitespace
            _ <- char('{')
            _ <- whitespace
            values <- deriveParsers[m.MirroredElemTypes](fieldNames)
            _ <- whitespace
            _ <- char('}')
          } yield values

        input =>
          parser(input) match
            case Right((tuple, rest)) =>
              try Right((m.fromTuple(tuple), rest))
              catch case e: Exception =>
                Left(ParseError(0, input, s"Error constructing instance: ${e.getMessage}"))
            case Left(err) => Left(err)

extension (s: String)
  def deserialize[A <: Product](using j: JsonDeserializer[A]): Either[ParseError, (A, String)] = summon[JsonDeserializer[A]].deserialize.apply(s)


