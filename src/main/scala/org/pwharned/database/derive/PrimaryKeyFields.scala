package org.pwharned.database.derive

import org.pwharned.database.hkd.*

import java.time.Instant
import java.util.UUID
import scala.compiletime.{constValue, erasedValue, summonInline}
import scala.deriving.Mirror

trait PrimaryKeyFields[T] {
  type Out <: Tuple
}

type IsPk[A] <: Boolean = A match
  case PrimaryKey[t]          => true
  case GeneratedPrimaryKey[t] => true
  case _                      => false

type FilterLabels[Ls <: Tuple, Ts <: Tuple] <: Tuple = (Ls, Ts) match
  case (EmptyTuple, EmptyTuple) => EmptyTuple
  case (lh *: lt, th *: tt) =>
    IsPk[th] match
      case true  => lh *: FilterLabels[lt, tt]
      case false => FilterLabels[lt, tt]

given [T](using m: Mirror.ProductOf[T]): PrimaryKeyFields[T] with {
  type Out = Tuple.Filter[
    m.MirroredElemTypes,
    [X] =>> X match {
      case PrimaryKey[t]          => true
      case GeneratedPrimaryKey[t] => true

      case _ => false
    }
  ]

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
        case PrimaryKey[t]          => (hl ->> hT) *: FilterPrimaryKey[tl, tT]
        case GeneratedPrimaryKey[t] => (hl ->> hT) *: FilterPrimaryKey[tl, tT]
        case _                      => FilterPrimaryKey[tl, tT]
    case _ => EmptyTuple

type PrimaryKeys[T <: Product] =
  FilterPrimaryKey[ElemLabels[T], ElemTypes[T]]

type Filter[Labels <: Tuple, Types <: Tuple, Keys <: Tuple] <: Tuple =
  (Labels, Types) match
    case (hl *: tl, hT *: tT) =>
      Keys match
        case hk *: kt =>
          hl =:= hk match
            case true => (hl ->> hT) *: Filter[tl, tT, kt]
            case _    => Filter[tl, tT, Keys]
        case _ => Filter[tl, tT, Keys]
    case _ => EmptyTuple

type ->>[K, V] = Tuple & (K, V)

type ElemLabels[T <: Product] = Mirror.ProductOf[T]#MirroredElemLabels

// 2) (Likewise, if you need the element‐type tuple)
type ElemTypes[T <: Product] = Mirror.ProductOf[T]#MirroredElemTypes

type Pick[T <: Product, Keys <: Tuple] =
  Filter[ElemLabels[T], ElemTypes[T], Keys]

/** Summon `List` of all labels in declaration order */

inline def buildPKsRec[Ls <: Tuple, Ts <: Tuple](
    values: Seq[Any]
): FilterPrimaryKey[Ls, Ts] =
  inline erasedValue[(Ls, Ts)] match
    case _: ((lh *: lt), (th *: tt)) =>
      inline erasedValue[th] match
        case _: GeneratedPrimaryKey[a] =>
          val labels = summonLabels[Ls]
          val idx = labels.indexOf(constValue[lh])
          val rawValue = summonInline[ValueDecoder[a]].fromAny(values(idx))
          val pkValue = GeneratedPrimaryKey(rawValue)
          // val head     = (constValue[lh], pkValue)
          val head = pkValue
          val tail = buildPKsRec[lt, tt](values)
          (head *: tail).asInstanceOf[FilterPrimaryKey[Ls, Ts]]
        case _: PrimaryKey[a] =>
          val labels = summonLabels[Ls]
          val idx = labels.indexOf(constValue[lh])
          val rawValue = summonInline[ValueDecoder[a]].fromAny(values(idx))
          val pkValue = PrimaryKey(rawValue)
          // val head     = (constValue[lh], pkValue)
          val head = pkValue
          val tail = buildPKsRec[lt, tt](values)
          (head *: tail).asInstanceOf[FilterPrimaryKey[Ls, Ts]]

        case _ =>
          buildPKsRec[lt, tt](values).asInstanceOf[FilterPrimaryKey[Ls, Ts]]

    case _: (EmptyTuple, EmptyTuple) =>
      EmptyTuple.asInstanceOf[FilterPrimaryKey[Ls, Ts]]

type ColumnsToTuple[T <: Tuple] <: Tuple = T match {
  case EmptyTuple => EmptyTuple
  case b *: tail  => b *: ColumnsToTuple[tail]
}
inline def summonLabels[Ls <: Tuple]: List[String] =
  inline erasedValue[Ls] match
    case _: (h *: t)   => constValue[h].toString :: summonLabels[t]
    case _: EmptyTuple => Nil

trait ValueDecoder[A] {
  def fromAny(v: Any): A
}

object ValueDecoder {
  given ValueDecoder[String] with { def fromAny(v: Any): String = v.toString }
  given ValueDecoder[Int] with {
    def fromAny(v: Any): Int = v match {
      case i: Int    => i
      case s: String => s.toInt
    }
  }
  given ValueDecoder[Long] with {
    def fromAny(v: Any): Long = v match {
      case l: Long   => l
      case s: String => s.toLong
    }
  }
  given ValueDecoder[java.util.UUID] with {
    def fromAny(v: Any): UUID = v match {
      case u: java.util.UUID => u
      case s: String         => java.util.UUID.fromString(s)
    }
  }
  given ValueDecoder[java.time.Instant] with {
    def fromAny(v: Any): Instant = v match {
      case i: java.time.Instant => i
      case s: String            => java.time.Instant.parse(s)
    }
  }
  // add more as needed
}

inline def primaryKeyNames[T <: Product](using
    m: Mirror.ProductOf[T]
): List[String] =
  summonLabels[FilterLabels[m.MirroredElemLabels, m.MirroredElemTypes]]
// Recurse over the tuple, setting each element on the JDBC Statement.
// Returns the next prepared-statement index.
inline def processColumns[T <: Tuple](
    columns: T,
    stmt: java.sql.PreparedStatement,
    idx: Int = 1
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

inline def extractPrimaryKeys[T <: Product](
    values: Seq[Any]
)(using m: Mirror.ProductOf[T]): PrimaryKeyFields[T]#Out =
  buildPKsRec[m.MirroredElemLabels, m.MirroredElemTypes](values)
    .asInstanceOf[PrimaryKeyFields[T]#Out]

@main
def t: Unit =
  case class Person[F[_]](
      name: F[GeneratedPrimaryKey[java.util.UUID]],
      age: F[Int]
  )

  type PersonName = PrimaryKeys[Person[Id]]
  val pkNames = primaryKeyNames[Persisted[Person]]
  println(pkNames)

  def runtimeKeys = Seq(java.util.UUID.randomUUID().toString)
  println(extractPrimaryKeys[Persisted[Person]](runtimeKeys))
