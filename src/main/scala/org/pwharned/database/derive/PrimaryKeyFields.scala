package org.pwharned.database.derive

import org.pwharned.database.hkd._

import scala.compiletime.{constValue, erasedValue, summonInline}
import scala.deriving.Mirror

trait PrimaryKeyFields[T] {
  type Out <: Tuple
}


given [T](using m: Mirror.ProductOf[T]): PrimaryKeyFields[T] with {
  type Out = Tuple.Filter[m.MirroredElemTypes, [X] =>> X match {
    case PrimaryKey[t] => true
    case GeneratedPrimaryKey[t] => true

    case _ => false
  }]

}

trait PrimaryKeyFieldLength[T] {
  type Out
}


given [T](using m: Mirror.ProductOf[T]): PrimaryKeyFieldLength[T] with {
  type Out = Tuple.Size[m.MirroredElemTypes]

}


type FilterPrimaryKey[Labels <: Tuple, Types <: Tuple] <: Tuple =
  (Labels, Types) match
    case (hl *: tl, hT *: tT) =>
      hT match
    // if this field‐type is a PrimaryKey[_], include (label ->> type)
      case PrimaryKey[t] => (hl ->> hT) *: FilterPrimaryKey[tl, tT]
      case _ => FilterPrimaryKey[tl, tT]
    case _ => EmptyTuple


type PrimaryKeys[T <: Product] =
  FilterPrimaryKey[ElemLabels[T], ElemTypes[T]]

type Filter[Labels<:Tuple, Types <:Tuple, Keys <: Tuple] <:Tuple =
  (Labels, Types) match
    case (hl *: tl, hT *: tT) =>
      Keys match
        case hk *: kt  =>  hl =:= hk  match
            case true => (hl ->> hT) *: Filter[tl, tT, kt]
            case _ => Filter[tl, tT, Keys]
        case _ => Filter[tl, tT, Keys]
    case _ => EmptyTuple

type ->>[K,V] = Tuple & (K,V)

type ElemLabels[T <: Product] = Mirror.ProductOf[T]#MirroredElemLabels

// 2) (Likewise, if you need the element‐type tuple)
type ElemTypes[T <: Product] = Mirror.ProductOf[T]#MirroredElemTypes

type Pick[T <: Product, Keys <: Tuple] =
  Filter[ElemLabels[T], ElemTypes[T], Keys]
/** Summon `List` of all labels in declaration order */
inline def summonLabels[Ls <: Tuple]: List[String] =
  inline erasedValue[Ls] match
    case _: (h *: t) => constValue[h].toString :: summonLabels[t]
    case _: EmptyTuple => Nil


inline def buildPKsRec[Ls <: Tuple, Ts <: Tuple](values: Seq[Any]): FilterPrimaryKey[Ls, Ts] =
  inline erasedValue[(Ls, Ts)] match
    case _: ((lh *: lt), (th *: tt)) =>
      inline erasedValue[th] match
        case _: GeneratedPrimaryKey[a] =>
          // 1) Find index of this label in the full labels list
          val labels = summonLabels[Ls]
          val idx = labels.indexOf(constValue[lh])
          // 2) Pull the runtime value, cast to `a`
          val rawValue = values(idx).asInstanceOf[a]
          // 3) Wrap it into your PK type
          val pkValue = GeneratedPrimaryKey(rawValue)
          // 4) Create the head pair (lh ->> th)
          val head = (constValue[lh], pkValue)
          val tail: FilterPrimaryKey[lt, tt] =
            buildPKsRec[lt, tt](values)
          // 5) Recurse for the tail
          (head *: tail).asInstanceOf[FilterPrimaryKey[Ls, Ts]]

        // Field `th` is PrimaryKey[a]
        case _: PrimaryKey[a] =>
          // 1) Find index of this label in the full labels list
          val labels = summonLabels[Ls]
          val idx = labels.indexOf(constValue[lh])
          // 2) Pull the runtime value, cast to `a`
          val rawValue = values(idx).asInstanceOf[a]
          // 3) Wrap it into your PK type
          val pkValue = PrimaryKey(rawValue)
          // 4) Create the head pair (lh ->> th)
          val head = (constValue[lh], pkValue)
          val tail: FilterPrimaryKey[lt, tt] =
            buildPKsRec[lt, tt](values)
          // 5) Recurse for the tail
          (head *: tail).asInstanceOf[FilterPrimaryKey[Ls, Ts]]

        // Field `th` not a PK: skip it
        case _ =>
          buildPKsRec[lt, tt](values).asInstanceOf[FilterPrimaryKey[Ls, Ts]]

    // No more fields
    case _: (EmptyTuple, EmptyTuple) =>
      EmptyTuple.asInstanceOf

type ColumnsToTuple[T <: Tuple] <: Tuple = T match {
  case EmptyTuple => EmptyTuple
  case b *: tail => b *: ColumnsToTuple[tail]
}
inline def primaryKeyNames[T <: Product]: List[String] =
  summonLabels[PrimaryKeys[T]]
// Recurse over the tuple, setting each element on the JDBC Statement.
// Returns the next prepared-statement index.
inline def processColumns[T <: Tuple](
                                       columns: T,
                                       stmt:   java.sql.PreparedStatement,
                                       idx:    Int = 1
                                     ): Int =
  inline erasedValue[T] match
    // Base case: no more columns to process
    case _: EmptyTuple =>
      idx

    // Recursive case: T = h *: t
    case _: (h *: t) =>
      // 1. Grab the head value at position `idx`
      val headValue = columns.head.asInstanceOf[h]

      // 2. Bind it to the statement (example uses setObject—you can specialize per type)
      stmt.setObject(idx, headValue)

      // 3. Recurse on the tail, bumping the index
      processColumns(columns.tail, stmt, idx + 1)

inline def extractPrimaryKeys[T <: Product](values: Seq[Any])(using m: Mirror.ProductOf[T]): PrimaryKeyFields[T]#Out =
  buildPKsRec[m.MirroredElemLabels, m.MirroredElemTypes](values)
    .asInstanceOf[PrimaryKeyFields[T]#Out]




@main
def t: Unit =
  case class Person[F[_]](name: F[PrimaryKey[String]], age: F[Int])

  type PersonName = PrimaryKeys[Person[Id]]
  

  def runtimeKeys =  Seq("Bob")
  println(extractPrimaryKeys[Person[Id]](runtimeKeys))

  val cols1 = (42, PrimaryKey("Hello"))
  val cols2 = ("world", true)
  val cols3 = (3.14, 'c')


