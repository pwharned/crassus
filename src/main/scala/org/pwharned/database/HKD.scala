package org.pwharned.database

import org.pwharned.database.HKD.PrimaryKey

import scala.language.postfixOps
import scala.language.implicitConversions

object HKD:
  /*
  // Your opaque types remain as before.
  opaque type PrimaryKey[X] = X

  object PrimaryKey:
    def apply[X](x: X): PrimaryKey[X] = x

    given [T]: Conversion[T, PrimaryKey[T]] = x => PrimaryKey(x)


  // ── Generic Type Aliases ──
  type Id[A] = A

  // When a “new” record is created from a request, its fields may be absent.
  type New[T[F[_]]] = T[Option]
  // When a record is persisted (e.g. from the DB), all fields exist.
  type Persisted[T[F[_]]] = T[Id]

  // ── Field Mapping via a Type Class ──

  // We want to compute a “new field” type for any given A.
  // For PrimaryKey[T], that should be Option[PrimaryKey[T]]
  // For Nullable[T], that should be Option[T]
  // For any other type A, that will be Option[A].
  trait NewFieldMapping[A]:
    type Out

  object NewFieldMapping:
    // The low-priority fallback instance.
    trait LowPriority:
      // Fallback for any type not handled by a more specific instance.
      given generic[A]: NewFieldMapping[A] with
        type Out = A

    object LowPriority extends LowPriority

    // Bring in the fallback given.
    export LowPriority.generic

    // More specific instance for PrimaryKey.
    given primaryKeyInstance[T]: NewFieldMapping[PrimaryKey[T]] with
      type Out = Option[T]

    // More specific instance for Nullable.
    given nullableInstance[T]: NewFieldMapping[Nullable[T]] with
      type Out = Option[T]

  // A convenient alias to extract the wrapped type.
  type NewField[A] = NewFieldMapping[A]#Out



  // ── A helper to derive a “new record” type if needed.
  // (Here, we’re assuming that your HKD design will eventually traverse each field
  // using the NewField mapping, for example via a generic derivation technique.
  // That code is not shown here, but NewField is available to use.)
  type NewRecord[T[_[_]]] = T[NewField]
*/


  sealed trait PrimaryKey[A]:
    def value: A

  object PrimaryKey:
    // A factory method to create a PrimaryKey without using a case class.
    def apply[A](a: A): PrimaryKey[A] = new PrimaryKey[A]:
      def value: A = a

    // Now, we define a given conversion from PrimaryKey[A] to A
    given [A]: Conversion[PrimaryKey[A], A] = (pk: PrimaryKey[A]) => pk.value


  sealed trait  Nullable[X]:
    def value: X

  object Nullable:
    def apply[X](x: X): Nullable[X] = x

    given [T]: Conversion[T, Nullable[T]] = x => Nullable(x)


      
  type Id[A] = A

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
    case Nullable[t] => Option[t]
    case _ => A

  type UpdatedField[A] = A match
    case PrimaryKey[t] => Option[PrimaryKey[t]]
    case Nullable[t] => Option[t]
    case _ => Option[A]
  type PersistedField[A] = A match
    case PrimaryKey[t] => t
    case Nullable[t] => Option[t]
    case _ => A
  type New[T[_[_]]] = T[NewField]
  type Updated[T[_[_]]] = T[UpdatedField]
  type Persisted[T[_[_]]] = T[PersistedField]


end HKD

given [A]: Conversion[A, Option[A]] = (a: A) => Some(a)

@main def demoHKD() = {
  import HKD.*
  case class User[F[_]](
                         id: F[PrimaryKey[Int]],
                         name: F[Nullable[String]], test: F[String]
                       )


  // Create a "new" user when receiving a request.
  // The primary key is absent (None) and the name is wrapped in Some.

  // Create a "persisted" user that might be fetched from the database.
  // The id is present as PrimaryKey(123) and the name is a plain String.
  val newUser: New[User] = User(None, "Alice", "Hello")
  val updatedUser: Updated[User] = User(None, "Alice", "Hello")
  val persistedUser: Persisted[User] = User(1, "Alice", "Hello")


  println(s"Persisted user (from DB): $persistedUser")



}