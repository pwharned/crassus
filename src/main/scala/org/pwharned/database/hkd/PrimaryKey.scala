package org.pwharned.database.hkd

sealed trait Constraint

package object ConstraintTypes {
  sealed trait DatabaseGeneratedPrimaryKey extends Constraint

  sealed trait RuntimeGeneratedPrimaryKey extends Constraint
  sealed trait DefaultValue extends Constraint
  sealed trait Nullable extends Constraint
}


sealed trait ColumnType[A, B <:Constraint]:
  def value: A
object KeyType:
  def apply[A, B<:Constraint](a: A): ColumnType[A, B] =
    new ColumnType[A,B]:
      val value: A = a


<<<<<<< HEAD:src/main/scala/org/pwharned/database/hkd/PrimaryKey.scala
sealed trait PrimaryKey[A] extends KeyType[A, GenerationMode.Runtime]:
  def value: A
=======

sealed trait PrimaryKey[A] extends ColumnType[A, ConstraintTypes.RuntimeGeneratedPrimaryKey]:
    def value: A
>>>>>>> main:src/main/scala/org/pwharned/sql/HKD/PrimaryKey.scala

object PrimaryKey:
  def apply[A](a: A): PrimaryKey[A] =
    new PrimaryKey[A]:
      val value: A = a


sealed trait GeneratedPrimaryKey[A] extends ColumnType[A, ConstraintTypes.DatabaseGeneratedPrimaryKey]:
  def value: A

object GeneratedPrimaryKey:
  def apply[A](a: A): GeneratedPrimaryKey[A] =
    new GeneratedPrimaryKey[A]:
      val value: A = a
<<<<<<< HEAD:src/main/scala/org/pwharned/database/hkd/PrimaryKey.scala
=======

sealed trait Default[X] extends ColumnType[X, ConstraintTypes.DefaultValue]:
  def value: X

object Default:
  def apply[X](x: X): Default[X] =
    new Default:
      val value: X = x

sealed trait Nullable[X] extends ColumnType[X, ConstraintTypes.Nullable]:
  def value: X

object Nullable:
  def apply[X](x: X): Nullable[X] =
    new Nullable[X]:
      val value: X = x

@main
def main: Unit =
  summon[PersistedField[PrimaryKey[String]] =:=  PersistedField[PrimaryKey[String]] ]
>>>>>>> main:src/main/scala/org/pwharned/sql/HKD/PrimaryKey.scala
