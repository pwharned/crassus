package org.pwharned.http.codec

trait JsonEncoder[T]:
  def encode(ent: T): String

object JsonEncoder:
  import java.nio.charset.StandardCharsets
  import scala.deriving.*
  import scala.compiletime.*



  inline val openBrace = "{"
  inline val closeBrace = "}"
  inline val comma = ","
  inline val colon = ":"

  // ─── Primitive Instances ─────────────────────────────────────────────────────

  given JsonEncoder[Int] with
    inline def encode(ent: Int): String = ent.toString

  given JsonEncoder[Double] with
    inline def encode(ent: Double): String = ent.toString

  given JsonEncoder[Long] with
    inline def encode(ent: Long): String = ent.toString

  given JsonEncoder[Boolean] with
    inline def encode(ent: Boolean): String = ent.toString

  given JsonEncoder[String] with
    inline def encode(ent: String): String =ent

  // ─── Recursive-safe Option Encoder ───────────────────────────────────────────

  given optionEncoder[T](using enc: => JsonEncoder[T]): JsonEncoder[Option[T]] with
    inline def encode(ent: Option[T]): String =
      ent match {
        case Some(value) => enc.encode(value)
        case None => "null"
      }

  given iteratorEncoder[T](using enc: => JsonEncoder[T]): JsonEncoder[Iterator[T]] with
    def encode(ent: Iterator[T]): String =
      val sb = ent.foldLeft(new StringBuilder("[")) { (sb, item) =>
        sb.append(enc.encode(item))
        sb.append(',')
      }
      if sb.length ==1 then  sb.append("]").toString else sb.setCharAt(sb.length - 1, ']').toString


  given encoder[T](using enc: => JsonEncoder[T]): JsonEncoder[List[T]] with
    inline def encode(ent: List[T]): String = "[" + ent.map(x => enc.encode(x)) + "]"
  // ─── Derivation ──────────────────────────────────────────────────────────────
  inline def isOptional[T]: Boolean =
    inline erasedValue[T] match
      case _: Option[?] => true
      case _ => false

  inline given derived[T<:Product](using m: Mirror.ProductOf[T]): JsonEncoder[T] =
    lazy val self: JsonEncoder[T] =
      val labels = getLabels[m.MirroredElemLabels]
      val encoderWithFlags = summonInstancesWithTypes[T, m.MirroredElemTypes](self)

      (ent: T) =>
        val fieldValues = (0 until ent.productArity).map(ent.productElement)

        val jsonPairs = labels.zip(encoderWithFlags).zip(fieldValues).collect {
          case ((name, (encoder, isOpt)), value) =>
            // Skip None values for optional fields
            if isOpt && value == None then None
            else {
              val encodedValue = encoder.asInstanceOf[JsonEncoder[Any]].encode(value)
              val encodedName = s"\"$name\""
              Some(encodedName ++ colon ++ encodedValue)
            }
        }.flatten
        
        if jsonPairs.isEmpty then
          openBrace ++ closeBrace
        else
          openBrace ++ jsonPairs.reduce((a, b) => a ++ comma ++ b) ++ closeBrace

    self

  // ─── Utilities ───────────────────────────────────────────────────────────────

  inline def summonInstances[T, Elems <: Tuple](self: => JsonEncoder[T]): List[JsonEncoder[?]] =
    inline erasedValue[Elems] match
      case _: (elem *: elems) => deriveOrSummon[T, elem](self) :: summonInstances[T, elems](self)
      case _: EmptyTuple      => Nil

  inline def summonInstancesWithTypes[T, Elems <: Tuple](self: => JsonEncoder[T]): List[(JsonEncoder[?], Boolean)] =
    inline erasedValue[Elems] match
      case _: (elem *: elems) =>
        val enc = deriveOrSummon[T, elem](self)
        val opt = isOptional[elem]
        (enc, opt) :: summonInstancesWithTypes[T, elems](self)
      case _: EmptyTuple => Nil

  inline def deriveOrSummon[T, Elem](self: => JsonEncoder[T]): JsonEncoder[Elem] =
    inline erasedValue[Elem] match
      case _: T => self.asInstanceOf[JsonEncoder[Elem]]
      case _    => summonInline[JsonEncoder[Elem]]

  inline def getLabels[T <: Tuple]: List[String] =
    constValueTuple[T].toList.asInstanceOf[List[String]]
