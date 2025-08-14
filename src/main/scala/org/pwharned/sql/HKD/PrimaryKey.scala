package org.pwharned.sql.HKD

sealed trait Mode

package object GenerationMode {
  sealed trait Database extends Mode

  sealed trait Runtime extends Mode
}


sealed trait KeyType[A, B <:Mode]:
  def value: A
object KeyType:
  def apply[A, B<:Mode](a: A): KeyType[A, B] =
    new KeyType[A,B]:
      val value: A = a

  final type DB[A] = KeyType[A, GenerationMode.Database]
  final type Runtime[A] = KeyType[A, GenerationMode.Runtime]

sealed trait PrimaryKey[A] extends KeyType[A, GenerationMode.Runtime]:
    def value: A

object PrimaryKey:
  def apply[A](a: A): PrimaryKey[A] =
    new PrimaryKey[A]:
      val value: A = a


sealed trait GeneratedPrimaryKey[A] extends KeyType[A, GenerationMode.Database]:
  def value: A

object GeneratedPrimaryKey:
  def apply[A](a: A): GeneratedPrimaryKey[A] =
    new GeneratedPrimaryKey[A]:
      val value: A = a

@main
def main: Unit =
  summon[PersistedField[PrimaryKey[String]] =:=  PersistedField[PrimaryKey[String]] ]