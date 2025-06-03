package org.pwharned.macros
import generated.PrimaryKey

import scala.language.postfixOps
case class User[F[_]](id: F[PrimaryKey[Int]], name: F[String])



object HKD {

  // ── Generic Type Aliases ──
  // We define an identity functor.
  type Id[A] = A

  // For any HKD-style type T (like User[F]), these generic aliases produce:
  // • New[T]  ≡ T[Option]: e.g. when deserializing a request where fields (like id) may be absent.
  // • Persisted[T] ≡ T[Id]: e.g. when all fields are available (from the database or after assignment).
  type New[T[F[_]]] = T[Option]
  type Persisted[T[F[_]]] = T[Id]


  // ── Natural Transformation Helper ──
  // A natural transformation from one effect F to another effect G.
  trait ~>[F[_], G[_]] {
    def apply[A](fa: F[A]): G[A]
  }

  object ~> {
    // Identity transformation: leaves the value unchanged.
    given idToId: Id ~> Id with {
      def apply[A](a: A): A = a
    }

    // Transformation from Id to Option: wraps a value in Some.
    given idToOption: Id ~> Option with {
      def apply[A](a: A): Option[A] = Some(a)
    }

 
  }


}

// ────────────────────────────────────────────
// Example usage
// ────────────────────────────────────────────

@main def demoHKD() = {
  import HKD.*

  // Create a "new" user when receiving a request.
  // The primary key is absent (None) and the name is wrapped in Some.
  val newUser: New[User] = User(None, Some("Alice"))
  val userString  = """{"name":"Alice"}"""
  println(userString.deserialize[New[User]])
  println(s"New user (deserialized from a request): $newUser")

  // Create a "persisted" user that might be fetched from the database.
  // The id is present as PrimaryKey(123) and the name is a plain String.
  val persistedUser: Persisted[User] = User(PrimaryKey(123), "Alice")
  println(s"Persisted user (from DB): $persistedUser")



}