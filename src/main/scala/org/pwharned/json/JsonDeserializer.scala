package org.pwharned.json

import org.pwharned.database.HKD.{Nullable, PrimaryKey}
import org.pwharned.parse.{Parse, ParseError, Parser, Primitives}

import scala.compiletime.*
import scala.deriving.*
import scala.quoted.*
import org.pwharned.database.HKD.~>.idToId
import org.pwharned.json

import  org.pwharned.`lazy`.Lazy

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


trait JsonDeserializer[T]:
  def deserialize: Parser[T]
  def isOptional: Boolean = false

  def defaultValue: Option[T] = None
object JsonDeserializer extends Parse:


  given JsonDeserializer[String] with
    def deserialize: Parser[String] = Primitives.quotedString

  given JsonDeserializer[Int] with
    def deserialize: Parser[Int] = Primitives.intParser

  given JsonDeserializer[Double] with
    def deserialize: Parser[Double] = Primitives.doubleParser
  given JsonDeserializer[Long] with
    def deserialize: Parser[Long] = Primitives.longParser
  given JsonDeserializer[Boolean] with
    def deserialize: Parser[Boolean] = Primitives.boolParser
  given JsonDeserializer[Float] with
    def deserialize: Parser[Float] = Primitives.floatParser
  given JsonDeserializer[java.util.UUID] with
    def deserialize: Parser[java.util.UUID] = Primitives.quotedString.map(x => java.util.UUID.fromString(x))
  // Wrap a parsed T into PrimaryKey[T]
  given [T](using underlying: JsonDeserializer[T]): JsonDeserializer[PrimaryKey[T]] with
    def deserialize: Parser[PrimaryKey[T]] =
      underlying.deserialize.map(PrimaryKey(_))
  given [T](using underlying: JsonDeserializer[T]): JsonDeserializer[Nullable[T]] with
    def deserialize: Parser[Nullable[T]] =
      underlying.deserialize.map(Nullable(_))




  given jsPrimitiveParser: JsonDeserializer[JsPrimitive] with
    def deserialize: Parser[JsPrimitive] =
      summon[JsonDeserializer[String]].deserialize
        .alt(summon[JsonDeserializer[Int]].deserialize)
        .alt(summon[JsonDeserializer[Boolean]].deserialize)
        .alt(summon[JsonDeserializer[Long]].deserialize)
        .alt(summon[JsonDeserializer[Double]].deserialize)
        .alt(summon[JsonDeserializer[Float]].deserialize)

  // For Option[T], first check for "null"; otherwise delegate.
  given [T](using underlying: JsonDeserializer[T]): JsonDeserializer[Option[T]] with
    override def isOptional: Boolean = true
    def deserialize: Parser[Option[T]] = input =>
      val trimmed = input.trim
      if trimmed.startsWith("null") then
        Right((Some(null.asInstanceOf[T]), trimmed.drop("null".length)))
      else
        underlying.deserialize(input) match {
          case Right((value, rest)) => Right((Some(value), rest))
          case Left(err)            => Left(err)
        }


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


  /** Parse `{ "a":.., "b":.., ... }` into a Map of raw AST nodes */
  private def objectAsMap: Parser[Map[String, JsonAst]] =
    for {
      _ <- char('{').token
      pairs <- (
        for {
          _ <- char('"')
          k <- stringInline
          _ <- char('"').token
          _ <- char(':').token
          v <- summon[JsonDeserializer[JsonAst]].deserialize
        } yield k -> v
        ).sepBy(comma.token)
      _ <- char('}').token
      _ <- whitespace
    } yield pairs.toMap

  private def renderJson(ast: JsonAst): String = ast match
    case JsonAst.JsValue(value) => value match
      case s: String => "\"" + s.replace("\"", "\\\"") + "\""
      case null => "null"
      case other => other.toString
    case JsonAst.Arr(items) =>
      items.iterator.map(renderJson).mkString("[", ",", "]")
    case JsonAst.Obj(fields) =>
      fields.iterator
        .map { case (k, v) =>
          "\"" + k.replace("\"", "\\\"") + "\":" + renderJson(v)
        }
        .mkString("{", ",", "}")

  // --- 3) Summon a JsonDeserializer[Any] for each element in a Tuple ------
  transparent inline def summonAllDesers[T <: Tuple]: List[JsonDeserializer[Any]] =
    inline erasedValue[T] match
      case _: (h *: t) =>
        // summon the h‐th deserializer *lazily*, then recurse
        summonInline[Lazy[JsonDeserializer[h]]].value
          .asInstanceOf[JsonDeserializer[Any]] ::
          summonAllDesers[t]
      case _: EmptyTuple =>
        Nil

  // --- 4) The order‐independent derived[T] -------------------------------
 

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
  inline def optKeyValuePair[A](key: String, valueParser: Parser[A]): Parser[Option[A]] =
    input =>
      if input.trim.startsWith("\"" + key + "\"") then
        keyValuePair(key, valueParser)(input).map { case (v, rest) => (Some(v), rest) }
      else
        Right((None, input))

  // Selects the correct parser based on field type.
  inline def fieldParser[h](key: String): Parser[h] = {
    inline erasedValue[h] match {

      case _: Option[t] =>

        optKeyValuePair(key, summonInline[Lazy[JsonDeserializer[t]]].value.deserialize).asInstanceOf[Parser[h]]
      case _: Product =>keyValuePair(key, summonInline[Lazy[JsonDeserializer[h]]].value.deserialize)

      case _ =>
        keyValuePair(key, summonInline[Lazy[JsonDeserializer[h]]].value.deserialize)
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


  given listDeserializer[A](using jd: Lazy[JsonDeserializer[A]]): JsonDeserializer[List[A]] =
    new JsonDeserializer[List[A]]:
      override def defaultValue: Option[List[A]] = Some(Nil)

      def deserialize: Parser[List[A]] =
        input =>
          // 1) parse raw AST array
          summon[JsonDeserializer[JsonAst]].deserialize(input) match
            case Left(err) =>
              Left(err)

            case Right((JsonAst.Arr(elems), restAfter)) =>
              elems.foldLeft(Right(Nil): Either[ParseError, List[A]]) {
                case (accE, astEl) =>
                  accE.flatMap { acc =>
                    val frag = renderJson(astEl)
                    jd.value.deserialize(frag) match
                      case Left(parseErr) =>
                        Left(parseErr)
                      case Right((a, leftover)) =>
                        if leftover.trim.nonEmpty then
                          Left(ParseError(
                            0,
                            frag,
                            s"Leftover in list element: '$leftover'"
                          ))
                        else
                          Right(acc :+ a)
                  }
              } match
                // 3) done!
                case Left(err) => Left(err)
                case Right(vList) => Right((vList, restAfter))

            case Right((other, _)) =>
              Left(ParseError(
                0,
                input,
                s"Expected JSON array but got AST node: $other"
              ))


  given mapDeserializer[A](using jd: Lazy[JsonDeserializer[A]]): JsonDeserializer[Map[String, A]] with {
    override def defaultValue: Option[Map[String, A]] = Some(Map.empty)

    def deserialize: Parser[Map[String, A]] =
      input =>
        objectAsMap(input) match {
          case Left(err) =>
            Left(err)

          case Right((raw, rest)) =>
            val folded: Either[ParseError, Map[String, A]] =
              raw.foldLeft(Right(Map.empty): Either[ParseError, Map[String, A]]) {
                case (accE, (k, ast)) =>
                  accE.flatMap { acc =>
                    val frag = renderJson(ast)
                    jd.value.deserialize(frag) match {
                      case Left(err) => Left(err)
                      case Right((a, leftover)) =>
                        if leftover.trim.isEmpty then
                          Right(acc + (k -> a))
                        else
                          Left(ParseError(
                            0,
                            frag,
                            s"Nested map‐entry '$k' had leftover: '$leftover'"
                          ))
                    }
                  }
              }

            // 3) done
            folded.map((_, rest))
        }
  }

  given jsonAstDeserializer: JsonDeserializer[JsonAst] =
    new JsonDeserializer[JsonAst] {
      override def deserialize: Parser[JsonAst] =

        // 1) primitive → JsValue
        val primP =
          summon[JsonDeserializer[JsPrimitive]].deserialize
            .map(JsonAst.JsValue.apply)
            .token

        // 2) array → Arr
        lazy val arrP =
          (for
            _ <- char('[').token
            xs <- deserialize.sepBy(comma)
            _ <- char(']')
          yield JsonAst.Arr(xs.toList))
            .token

        // 3) object → Obj
        lazy val objP =
          (for
            _ <- char('{').token
            pairs <- (for
              _ <- char('"')
              k <- stringInline
              _ <- char('"')
              _ <- char(':')
              v <- deserialize
            yield k -> v)
              .sepBy(comma)
            _ <- char('}').token
          yield JsonAst.Obj(pairs.toMap))
            .token

        objP.alt(arrP).alt(primP)
    }


  transparent inline def ordered[T <: Product](using m: Mirror.ProductOf[T]): JsonDeserializer[T] =
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



  inline given derived[T <: Product](using
                                                 m: Mirror.ProductOf[T]
                                                ): JsonDeserializer[T] =
    new JsonDeserializer[T]:
      def deserialize: Parser[T] =
        input =>
          // 1) parse the entire object into a Map[String,JsonAst]
          objectAsMap(input) match
            case Left(err) =>
              Left(err)

            case Right((astMap, rest)) =>
              // 2) summon all the element-type deserializers as erased to Any
              val labels = constValueTuple[m.MirroredElemLabels].toIArray.toList.map(_.toString)
              val desersAny = summonAllDesers[m.MirroredElemTypes]

              // 3) for each (label, deser) pull ast, re-render, re-parse
              def go(
                      keys: List[String],
                      ds: List[JsonDeserializer[Any]],
                      acc: List[Any]
                    ): Either[ParseError, List[Any]] =
                (keys, ds) match
                  case (Nil, Nil) =>
                    Right(acc.reverse)

                  case (key :: ktail, d :: dtail) =>
                    astMap.get(key) match
                      case None if d.isOptional =>go(ktail, dtail, None :: acc)
                      case None =>
                        Left(ParseError(0, input, s"Missing required field '$key' ${d.defaultValue}"))

                      case Some(fieldAst) =>
                        // re-render the fragment back to JSON
                        val fragment = renderJson(fieldAst)
                        d.deserialize(fragment) match
                          case Left(err) =>
                            Left(err)

                          case Right((value, leftover)) =>
                            if leftover.trim.nonEmpty then
                              Left(ParseError(0, fragment, s"Unconsumed input for field '$key': '$leftover'"))
                            else
                              go(ktail, dtail, value :: acc)

                  case _ =>
                    // mismatch in labels vs. desers
                    Left(ParseError(0, input, s"Internal error: label/deser mismatch"))

              // 4) run it, then rebuild the product instance
              go(labels, desersAny, Nil) match
                case Left(err) =>
                  Left(err)

                case Right(values) =>
                  // pack into a Tuple and then into T
                  try
                    val tuple0 = values.foldRight(EmptyTuple: Tuple){ (v, t) => v *: t } .asInstanceOf[m.MirroredElemTypes]
                    Right((m.fromTuple(tuple0), rest))
                  catch
                    case ex: Exception =>
                      Left(ParseError(0, input, s"Construction failed: ${ex.getMessage}"))
