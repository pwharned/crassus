package org.pwharned.utils

import org.pwharned.sql.HKD._
import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonInline}
import scala.util.Random

/**
 * A typeclass that knows how to produce a random T. 
 * You can summon it for primitives, options, PrimaryKey[T], nullable, or any
 * Product (i.e. case‐class) whose fields themselves have RandomValue instances.
 */
trait RandomValue[T]:
  def generate: T

object RandomValue:
  /** summon helper */
  def apply[T](using rv: RandomValue[T]): RandomValue[T] = rv

  //–– 1) Base instances for “plain” types
  given RandomValue[String] with
    def generate = Random.alphanumeric.take(10).mkString
    
  given RandomValue[java.time.Instant] with
    def generate = java.time.Instant.now()

  given RandomValue[Int] with
    def generate = Random.nextInt(100)

  given RandomValue[Long] with
    def generate = Random.nextLong()

  given RandomValue[Double] with
    def generate = Random.nextDouble()

  given RandomValue[Float] with
    def generate = Random.nextFloat()

  given RandomValue[Boolean] with
    def generate = Random.nextBoolean()
  given list[T](using underlying: RandomValue[T]): RandomValue[List[T]] with
    def generate = List(underlying.generate)

  given RandomValue[java.util.UUID] with
    def generate = java.util.UUID.randomUUID()

  //–– 2) Wrapper‐type instances for HKD
  given [T](using rv: RandomValue[T]): RandomValue[Option[T]] with
    def generate = Some(rv.generate)

  given [T](using rv: RandomValue[T]): RandomValue[PrimaryKey[T]] with
    def generate = PrimaryKey(rv.generate)
  given [T](using rv: RandomValue[T]): RandomValue[GeneratedPrimaryKey[T]] with
    def generate = GeneratedPrimaryKey(rv.generate)

  given [T](using rv: RandomValue[T]): RandomValue[Nullable[T]] with
    def generate = Nullable(rv.generate)

  // If you need PersistedField or UpdatedField, uncomment / copy‐in:
  // given [T](using rv: RandomValue[T]): RandomValue[PersistedField[T]] with
  //   def generate = PersistedField(rv.generate)
  //
  // given [T](using rv: RandomValue[T]): RandomValue[UpdatedField[T]] with
  //   def generate = UpdatedField(rv.generate)


  //–– 3) Tuple‐level recursion (for case‐class elements)
  inline given derivedTuple[X <: Tuple]: RandomValue[X] =
    (
      inline erasedValue[X] match
        case _: EmptyTuple =>
          new RandomValue[EmptyTuple]:
            def generate = EmptyTuple

        case _: (h *: t) =>
          val headGen = summonInline[RandomValue[h]]
          val tailGen = summonInline[RandomValue[t]]
          new RandomValue[h *: t]:
            def generate = headGen.generate *: tailGen.generate
      ).asInstanceOf[RandomValue[X]] // <= cast

  //–– 4) Case‐class (Product) derivation
  inline given derivedProduct[CC <: Product](using
                                             m: Mirror.ProductOf[CC],
                                             rv: RandomValue[m.MirroredElemTypes]
                                            ): RandomValue[CC] =
    new RandomValue[CC]:
      def generate: CC =
        // generate the tuple of all fields…
        val elems = rv.generate
        // …then build the case‐class
        m.fromProduct(elems)


/**
 * A tiny shim if you really want to keep your `RandomGenerator[T<:Product]`
 * name and interface. It simply delegates to RandomValue[T].
 */
trait RandomGenerator[T <: Product]:
  def generate: T

object RandomGenerator:
  def apply[T <: Product](using rg: RandomValue[T]): RandomValue[T] = rg

  inline given derived[T <: Product](using rv: RandomValue[T]): RandomGenerator[T] =
    new RandomGenerator[T]:
      def generate = rv.generate
