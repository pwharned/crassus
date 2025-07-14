package org.pwharned.utils

import java.io.File
import scala.compiletime.{constValue, constValueTuple, erasedValue, summonInline}
import scala.deriving.Mirror
import scala.io.Source
import scala.util.{Failure, Success, Try}
object EnvLoader {

  /**
   * Helper inline method to get field names from a case class
   */
  inline def fieldNames[T](using m: Mirror.Of[T]): List[String] = {

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
  
    // ... keep your existing fieldNames and loadFromEnvFile functions

    inline def loadFromSystemEnv[T](using m: Mirror.ProductOf[T]): Either[String, T] = {
      val fields = fieldNames[T]

      Try {
        val fieldValues = fields.map { fieldName =>
          val envVarName = fieldName.toUpperCase
          sys.env.getOrElse(envVarName,
            throw new RuntimeException(s"Missing $envVarName in system environment"))
        }

        m.fromProduct(Tuple.fromArray(fieldValues.toArray))
      } match {
        case Success(instance) => Right(instance)
        case Failure(exception) => Left(s"Failed to load from system environment: ${exception.getMessage}")
      }
    }

    inline def loadFromFileOrEnv[T](filePath: String)(using m: Mirror.ProductOf[T]): Either[String, T] = {
      loadFromEnvFile[T](filePath) match {
        case Right(instance) => Right(instance)
        case Left(_) => loadFromSystemEnv[T]
      }
    }
}
// Usage example

