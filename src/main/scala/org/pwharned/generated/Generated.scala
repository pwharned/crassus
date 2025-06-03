
package generated
import org.pwharned.database.HKD._


case class user[F[_]] ( id: F[PrimaryKey[Int]],name: F[String],test: F[Nullable[String]])
