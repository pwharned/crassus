
package generated
opaque type PrimaryKey[X] = X

object PrimaryKey:
  def apply[X](x: X): PrimaryKey[X] = x

  given [T]: Conversion[T, PrimaryKey[T]] = x => PrimaryKey(x)


case class user ( id: PrimaryKey[Int],name: String,test: Option[String])
