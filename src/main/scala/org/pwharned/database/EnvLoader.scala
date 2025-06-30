package org.pwharned.database
import scala.io.Source
import scala.util.{Try, Success, Failure}
import java.io.File
import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonInline, constValue}
object EnvLoader {

  /**
   * Helper inline method to get field names from a case class
   */
  inline def fieldNames[T](using m: Mirror.Of[T]): List[String] = {
    import scala.compiletime.constValueTuple

    inline erasedValue[m.MirroredElemLabels] match {
      case _: (head *: tail) =>
        constValueTuple[m.MirroredElemLabels].productIterator.toList.map(_.toString)
      case _: EmptyTuple => Nil
    }
  }


  inline def loadFromEnvFile[T](filePath: String)(using m: Mirror.ProductOf[T]): Either[String, T] = {
    // Get field names at compile time
    val fields = fieldNames[T]

    Try {
      // Read the file
      val source = Source.fromFile(filePath)
      try {
        // Parse key-value pairs
        val envMap = source.getLines()
          .map(_.trim)
          .filter(line => line.nonEmpty && !line.startsWith("#"))
          .map { line =>
            val parts = line.split("=", 2)
            if (parts.length == 2) (parts(0).trim, parts(1).trim)
            else throw new RuntimeException(s"Invalid line format: $line")
          }
          .toMap

        // Get values for each field by converting field name to ENV_VAR format
        val fieldValues = fields.map { fieldName =>
          val envVarName = fieldName.toUpperCase
          envMap.getOrElse(envVarName,
            throw new RuntimeException(s"Missing $envVarName in env file"))
        }

        // Create the instance using the Mirror
        m.fromProduct(Tuple.fromArray(fieldValues.toArray))
      } finally {
        source.close()
      }
    } match {
      case Success(instance) => Right(instance)
      case Failure(exception) => Left(s"Failed to load instance: ${exception.getMessage}")
    }
  }
}
// Usage example

