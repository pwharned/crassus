package org.pwharned.codec
trait JsonEncoder[T]:
  def encode(ent: T): Array[Byte]

object JsonEncoder:
  import java.nio.charset.StandardCharsets
  import scala.deriving.*
  import scala.compiletime.*


  inline def openBrace: Array[Byte] = Array(123) // "{"

  inline def closeBrace: Array[Byte] = Array(125) // "}"

  inline def comma: Array[Byte] = Array(44) // ","

  inline def colon: Array[Byte] = Array(58) // ":"



  // ─── Primitive Instances ─────────────────────────────────────────────────────

  given JsonEncoder[Int] with
    inline def encode(ent: Int): Array[Byte] = ent.toString.getBytes(StandardCharsets.UTF_8)

  given JsonEncoder[Double] with
    inline def encode(ent: Double): Array[Byte] = ent.toString.getBytes(StandardCharsets.UTF_8)

  given JsonEncoder[Long] with
    inline def encode(ent: Long): Array[Byte] = ent.toString.getBytes(StandardCharsets.UTF_8)

  given JsonEncoder[Boolean] with
    inline def encode(ent: Boolean): Array[Byte] = ent.toString.getBytes(StandardCharsets.UTF_8)

  given JsonEncoder[String] with
    inline def encode(ent: String): Array[Byte] =
      ("\"" + ent.replaceAll("\"", "\\\"") + "\"").getBytes(StandardCharsets.UTF_8)

  // ─── Recursive-safe Option Encoder ───────────────────────────────────────────

  given optionEncoder[T](using enc: => JsonEncoder[T]): JsonEncoder[Option[T]] with
    inline def encode(ent: Option[T]): Array[Byte] =
      ent match {
        case Some(value) => enc.encode(value)
        case None => "null".getBytes(StandardCharsets.UTF_8)
      }

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
              val encodedName = s"\"$name\"".getBytes(StandardCharsets.UTF_8)
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
