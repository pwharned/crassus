package org.pwharned.macros
import scala.deriving.*
import scala.compiletime.*




trait FilterUnion[T, Exclude] {
  type Out
}

given [T, Exclude](using m: Mirror.ProductOf[T]): FilterUnion[T, Exclude] with {
  type Out = Tuple.Filter[m.MirroredElemTypes, [X] =>> X match {
    case Exclude => Nothing
    case _ => X
  }]
}




trait SqlDelete[T]:
  def deleteStatement: String
  def bindValues(obj: T): Seq[Any] // Extract values separately

object SqlDelete:
  inline given derive[T <: Product](using m: Mirror.ProductOf[T]): SqlDelete[T] =
    new SqlDelete[T]:
      def deleteStatement: String =
        val tableName = constValue[m.MirroredLabel]

        val primaryKey = PrimaryKeyExtractor.getPrimaryKey[T]

        s"SELECT * FROM FINAL TABLE( DELETE FROM $tableName WHERE $primaryKey = ?)"

      def bindValues[T](primaryKeyData: Map[String, String]): Seq[Any] =
        val primaryKeys = PrimaryKeyExtractor.getPrimaryKey[T] // Compile-time primary key extraction

        // Ensure values are correctly ordered based on primary key definitions
        primaryKeys.map { key =>
          primaryKeyData.getOrElse(key, throw new IllegalArgumentException(s"Missing primary key: $key"))
        }



