package org.pwharned.sql.HKD

import org.pwharned.json.{JsonDeserializer, deserialize}

import scala.deriving.Mirror
import scala.language.{implicitConversions, postfixOps}


extension [A](pk: PrimaryKey[A])
  def value: A = pk.value // unwraps to original A

type Id[A] = A
type IdHKD[T] = [F[_]] =>> T
type UnHKD[H[_[_]]] = H[[X] =>> Id[X]]


// ── Natural Transformation Helper ──
trait ~>[F[_], G[_]]:
  def apply[A](fa: F[A]): G[A]

object ~> {
  given idToId: Id~> Id with
    def apply[A](a: A): A = a

  given idToOption: Id~> Option with
    def apply[A](a: A): Option[A] = Some(a)
}


type NewField[A] = A match
  case PrimaryKey[t] => Option[PrimaryKey[t]]
  case GeneratedPrimaryKey[t] => Option[GeneratedPrimaryKey[t]]
  case Nullable[t] => Option[t]
  case Default[t] => Option[t]
  case _ => A


type OptionalField[A] = A match
  case PrimaryKey[t] => Option[t]
  case GeneratedPrimaryKey[t] => Option[t]
  case Nullable[t] => Option[t]
  case Default[t] => Option[t]
  case Option[t] => Option[t]
  case _ => Option[A]


type UpdatedField[A] = A match
  case PrimaryKey[t] => Option[PrimaryKey[t]]
  case GeneratedPrimaryKey[t] => Option[GeneratedPrimaryKey[t]]
  case Nullable[t] => Option[t]
  case Default[t] => Option[t]
  case _ => Option[A]

type PersistedField[A] = A match
  case PrimaryKey[t] => PrimaryKey[t]
  case GeneratedPrimaryKey[t] => GeneratedPrimaryKey[t]
  case Nullable[t] => Option[t]
  case Default[t] => Option[t]
  case _ => A
type New[T[_[_]]] = T[NewField]
type Updated[T[_[_]]] = T[UpdatedField]
type Persisted[T[_[_]]] = T[PersistedField]
type Optional[T[_[_]]] = T[OptionalField]


object Conversions:
  given [A]: Conversion[A, Option[A]] = (a: A) => Some(a)




// …and so on for Default, PrimaryKey, etc.
