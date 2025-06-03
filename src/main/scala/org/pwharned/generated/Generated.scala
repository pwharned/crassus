
package generated
opaque type PrimaryKey[X] = X

object PrimaryKey:
  def apply[X](x: X): PrimaryKey[X] = x

  given [T]: Conversion[T, PrimaryKey[T]] = x => PrimaryKey(x)


case class user[F[_]] ( id: F[PrimaryKey[Int]],name: F[String],test: F[Option[String]])
