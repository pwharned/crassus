
package generated
import scala.annotation.StaticAnnotation
class PrimaryKey extends StaticAnnotation


case class user (@PrimaryKey id: Integer,name: String,test: Option[String])
