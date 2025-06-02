package org.pwharned.macros
import scala.compiletime.*
import scala.deriving.*
import generated.PrimaryKey

object PrimaryKeyExtractor:
  inline def getPrimaryKey[T <: Product](using m: Mirror.ProductOf[T]): Seq[String] =
    getPrimaryKeyImpl[m.MirroredElemLabels, m.MirroredElemTypes]

  inline def getPrimaryKeyImpl[L <: Tuple, T <: Tuple]: Seq[String] =
    val labels = tupleToList[L]
    val primaryKeyLabels = tupleFilterKeys[T](labels)
    primaryKeyLabels

  inline def tupleToList[T <: Tuple]: List[String] =
    inline erasedValue[T] match
      case _: (h *: t) => constValue[h].toString :: tupleToList[t]
      case _ => Nil

  inline def tupleFilterKeys[T <: Tuple](labels: List[String]): Seq[String] =

    inline erasedValue[T] match
      case _: (PrimaryKey[Int] *: t) => labels.head +: tupleFilterKeys[t](labels.tail)
      case _: (_ *: t) => tupleFilterKeys[t](labels.tail)
      case _ => Nil





