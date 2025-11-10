package org.pwharned.sql.HKD

import org.pwharned.sql.HKD


trait FieldReducer[T] {
  type Out
  def unwrap(t: T): Out    // for serialization
  def wrap(o: Out): T      // for deserialization
}
object FieldReducer {
  type Aux[T,R] = FieldReducer[T] { type Out = R }

  // 1) Generated[PrimaryKey[A]]  ⇒  PrimaryKey[A]
  // 2) PrimaryKey[A] ⇒ A
  given generatedPrimaryKey[A]: Aux[HKD.GeneratedPrimaryKey[A], A] =
  new FieldReducer[GeneratedPrimaryKey[A]] {
    type Out = A
    def unwrap(pk: GeneratedPrimaryKey[A]): A = pk.value
    def wrap(a: A): GeneratedPrimaryKey[A]  = GeneratedPrimaryKey(a)
  }


  // 2) PrimaryKey[A] ⇒ A
  given primaryKey[A]: Aux[PrimaryKey[A], A] =
  new FieldReducer[PrimaryKey[A]] {
    type Out = A
    def unwrap(pk: PrimaryKey[A]): A = pk.value
    def wrap(a: A): PrimaryKey[A]  = PrimaryKey(a)
  }

  // 3) Default[A] ⇒ Option[A]
  given default[A]: Aux[Default[A], Option[A]] =
  new FieldReducer[Default[A]] {
    type Out = Option[A]
    def unwrap(d: Default[A]): Option[A] = Option(d.value)
    def wrap(o: Option[A]): Default[A]  = Default(o.get)
  }

  // 4) Nullable[A] ⇒ Option[A]
  given nullable[A]: Aux[Nullable[A], Option[A]] =
  new FieldReducer[Nullable[A]] {
    type Out = Option[A]
    def unwrap(n: Nullable[A]): Option[A] = Option(n.value)
    def wrap(o: Option[A]): Nullable[A]   = Nullable(o.get)
  }

  given persistedField[A, R](using
                             fr: FieldReducer.Aux[A, R]
                            ): Aux[PersistedField[A], R] =
    new FieldReducer[PersistedField[A]] {
      type Out = R

      def unwrap(pf: PersistedField[A]): R =
        // PersistedField[A] is just a type-alias to one of
        //   Generated[A], PrimaryKey[A], Option[A], etc.
        // so we can safely cast it back to A and then call fr.unwrap
        fr.unwrap(pf.asInstanceOf[A])

      def wrap(r: R): PersistedField[A] =
        // build the original A via fr.wrap(r), then view it as PersistedField[A]
        fr.wrap(r).asInstanceOf[PersistedField[A]]
    }

  // 5) Fallback: anything else ⇒ itself

}
