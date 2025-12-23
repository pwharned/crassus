package org.pwharned.config

import scala.io.Source
import scala.util.{Try, Success, Failure}
import java.io.File
import scala.deriving.Mirror
import scala.compiletime.constValueTuple

import scala.compiletime.{erasedValue, summonInline, constValue}
object EnvLoader {

  /** Helper inline method to get field names from a case class
    */
  inline def fieldNames[T](using m: Mirror.Of[T]): List[String] = {

    inline erasedValue[m.MirroredElemLabels] match {
      case _: (head *: tail) =>
        constValueTuple[m.MirroredElemLabels].productIterator.toList
          .map(_.toString)
      case _: EmptyTuple => Nil
    }
  }

  inline def loadFromEnvFile[T](
      filePath: String
  )(using m: Mirror.ProductOf[T]): Either[String, T] = {
    // Get field names at compile time
    val fields = fieldNames[T]
    val className = constValue[m.MirroredLabel]

    Try {
      // Read the file
      val source = Source.fromFile(filePath)
      try {
        // Parse key-value pairs
        val envMap = source
          .getLines()
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
          val envVarName = className.toUpperCase + "_" + fieldName.toUpperCase
          envMap.getOrElse(
            envVarName,
            throw new RuntimeException(s"Missing $envVarName in env file")
          )
        }

        // Create the instance using the Mirror
        m.fromProduct(Tuple.fromArray(fieldValues.toArray))
      } finally {
        source.close()
      }
    } match {
      case Success(instance) => Right(instance)
      case Failure(exception) =>
        Left(s"Failed to load instance: ${exception.getMessage}")
    }
  }

  inline def loadFromSystemEnv[T](using
      m: Mirror.ProductOf[T]
  ): Either[String, T] = {
    val fields = fieldNames[T]
    val className = constValue[m.MirroredLabel]

    Try {
      val fieldValues = fields.map { fieldName =>
        val envVarName = className.toUpperCase + "_" + fieldName.toUpperCase
        sys.env.get(envVarName.toUpperCase) match {
          case Some(value) => value
          case None =>
            sys.env.get(envVarName.toLowerCase) match {
              case Some(value) => value
              case None =>
                throw new RuntimeException(
                  s"Missing $envVarName in system environment"
                )
            }
        }
      }

      m.fromProduct(Tuple.fromArray(fieldValues.toArray))
    } match {
      case Success(instance) => Right(instance)
      case Failure(exception) =>
        Left(s"Failed to load from system environment: ${exception.getMessage}")
    }
  }

  inline def loadFromFileOrEnv[T](
      filePath: String
  )(using m: Mirror.ProductOf[T]): Either[String, T] = {
    loadFromEnvFile[T](filePath) match {
      case Right(instance) => Right(instance)
      case Left(_)         => loadFromSystemEnv[T]
    }
  }

}
// Usage example
