package org.pwharned.sql.database

import org.pwharned.json.{JsonDeserializer, deserialize}
import org.pwharned.sql.database.HKD.PrimaryKey

import scala.deriving.Mirror
import scala.language.{implicitConversions, postfixOps}

object HKD:
 
  sealed trait PrimaryKey[A]:
    def value: A

  object PrimaryKey:
    // A factory method to create a PrimaryKey without using a case class.
    def apply[A](a: A): PrimaryKey[A] = new PrimaryKey[A]:
      def value: A = a

    // Now, we define a given conversion from PrimaryKey[A] to A
    given [A]: Conversion[PrimaryKey[A], A] = (pk: PrimaryKey[A]) => pk.value

    given [A]: Conversion[A, PrimaryKey[A]] = (pk: A) => PrimaryKey(pk)

  sealed trait  Nullable[X]:
    def value: X

  object Nullable:
    def apply[X](x: X): Nullable[X] = Nullable(x)

    given [T]: Conversion[T, Nullable[T]] = x => Nullable(x)
    given [T]: Conversion[Nullable[T], T] = x => x.value

  sealed trait Default[X]:
    def value: X

  object Default:
    def apply[X](x: X): Default[X] = x

    given [T]: Conversion[T, Default[T]] = x => Default(x)

    given [T]: Conversion[Default[T], T] = x => x.value

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
    case PrimaryKey[t] => Option[t]
    case Nullable[t] => Option[t]
    case Default[t] => Option[t]
    case _ => A
  
  
  type OptionalField[A] = A match
    case PrimaryKey[t] => Option[t]
    case Nullable[t] => Option[t]
    case Default[t] => Option[t]
    case Option[t] => Option[t]
    case _ => Option[A]
  type UpdatedField[A] = A match
    case PrimaryKey[t] => Option[t]
    case Nullable[t] => Option[t]
    case Default[t] => Option[t]
    case _ => Option[A]
  type PersistedField[A] = A match
    case PrimaryKey[t] => PrimaryKey[t]
    case Nullable[t] => Option[t]
    case Default[t] => Option[t]
    case _ => A
  type New[T[_[_]]] = T[NewField]
  type Updated[T[_[_]]] = T[UpdatedField]
  type Persisted[T[_[_]]] = T[PersistedField]
  type Optional[T[_[_]]] = T[OptionalField]


  object Conversions:
    given [A]: Conversion[A, Option[A]] = (a: A) => Some(a)


end HKD




// …and so on for Default, PrimaryKey, etc.
