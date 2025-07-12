package org.pwharned.utils

import scala.compiletime.constValueTuple
import scala.deriving.Mirror

type FieldsOf[T] =
  Tuple.Zip[
    Mirror.ProductOf[T]#MirroredElemLabels,
    Mirror.ProductOf[T]#MirroredElemTypes
  ]

type Concat[X<:Tuple, Y<:Tuple] <: Tuple =
  X match
    case EmptyTuple => Y
    case h *: t      => h *: Concat[t,Y]

type Distinct[X<:Tuple] <: Tuple =
  X match
    case EmptyTuple        => EmptyTuple
    case (l,t) *: tail     => (l,t) *:
      Distinct[
        Tuple.Filter[tail, [e] =>> e match
          // if tail-entry has same label l, drop it
          case (`l`,_) => false
          case _       => true
        ]
      ]

type UnionFields[A<:Product, B<:Product] =
  Distinct[ Concat[ FieldsOf[A], FieldsOf[B] ] ]

type FieldType[P] = P match
  case (_lbl, t) => t

type UnionTypes[A<:Product, B<:Product] =
  Tuple.Map[ UnionFields[A, B], FieldType ]

// —————————————————————————————————————————————————————
// And here’s an inline join that *actually* builds the tuple
// —————————————————————————————————————————————————————
private def dedupe[K, V](xs: List[(K, V)]): List[(K, V)] =
  xs.groupBy(_._1).view.mapValues(_.head._2).toList

inline def join[A<:Product, B<:Product](a: A, b: B)
                                       (using
                                        ma: Mirror.ProductOf[A],
                                        mb: Mirror.ProductOf[B]
                                       ): UnionTypes[A,B] =

  // 1) at runtime pull out the names & values
  val namesA = constValueTuple[ma.MirroredElemLabels].toList.asInstanceOf[List[String]]
  val valsA  = Tuple.fromProductTyped(a).toList
  val namesB = constValueTuple[mb.MirroredElemLabels].toList.asInstanceOf[List[String]]
  val valsB  = Tuple.fromProductTyped(b).toList

  // 2) zip + dedupe by name (keep first‐seen)
  val merged = (namesA zip valsA) ++ (namesB zip valsB)
  val unique = dedupe(merged)

  // 3) rebuild a Scala tuple of the right arity
  //    (you can unroll as many cases as you need)
  unique match
    case Nil              => EmptyTuple.asInstanceOf[UnionTypes[A,B]]
    case (n1,v1)::Nil     => Tuple1(v1).asInstanceOf[UnionTypes[A,B]]
    case (n1,v1)::(n2,v2)::Nil =>
      (v1, v2).asInstanceOf[UnionTypes[A,B]]
    case (n1,v1)::(n2,v2)::(n3,v3)::Nil =>
      (v1, v2, v3).asInstanceOf[UnionTypes[A,B]]
// …etc…

// ————————————————————————————————
// usage
// ————————————————————————————————
case class Foo(i: Int, s: String)
case class Bar(s: String, d: Double)

@main def testJoin() =
  val f = Foo(1, "hello")
  val g = Bar("hello", 2.0)

  // compiler knows join(f,g): (Int,String,Double)
  val t  = join(f,g)
  println(t)  // (1, "hello", 2.0)
