package org.pwharned.macros

import scala.compiletime.{erasedValue, summonInline}

import scala.util.Try

trait FromString[A]:
  def parse(s: String): A

object FromString:
  given FromString[Int] with
    def parse(s: String): Int = s.toInt

  given FromString[String] with
    def parse(s: String): String = s

inline def listToTuple[T <: Tuple](list: List[String]): T = {
  inline erasedValue[T] match
    case _: EmptyTuple =>
      EmptyTuple.asInstanceOf[T]
    case _: (h *: t) =>
      // Convert the head string to type h
      val head: h = summonInline[FromString[h]].parse(list.head)

      // Recursively convert the remainder of the list to type t
      val tail: t = listToTuple[t](list.tail)
      (head *: tail).asInstanceOf[T]
}
def toTuple(list: List[String]): Tuple = list match {
  case head :: tail =>
    // Try to parse the head element into an Int,
    // using summonInline to obtain the given FromString[Int]
    val maybeInt: Option[Int] =
      Try { summonInline[FromString[Int]].parse(head) }.toOption

    maybeInt match {
      case Some(value) =>
        // Prepend the parsed Int to the tuple produced by the tail.
        value *: toTuple(tail)
      case None =>
        // If the head cannot be parsed, throw an exception.
        summonInline[FromString[Int]].parse(head) *: toTuple(tail)
    }
  case Nil =>
    // Base case: an empty list corresponds to an empty tuple.
    EmptyTuple
}

inline def listToTupleT[A,T <: Tuple](list: List[A]): T = {
  inline erasedValue[T] match
    case _: EmptyTuple =>
      EmptyTuple.asInstanceOf[T]
    case _: (h *: t) =>
      // Convert the head string to type h

      // Recursively convert the remainder of the list to type t
      val tail: t = listToTupleT[Any,t](list.tail)
      (list.head *: tail).asInstanceOf[T]
}