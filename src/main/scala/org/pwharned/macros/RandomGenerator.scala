package org.pwharned.macros

import scala.deriving.Mirror
import scala.util.Random
import scala.deriving.*
import scala.compiletime.*
import scala.reflect.ClassTag

transparent inline def generateRandomValue[T]: T =
  inline erasedValue[T] match
    case _: String => Random.alphanumeric.take(10).mkString.asInstanceOf[T]
    case _: Option[String] => Some(Random.alphanumeric.take(10).mkString).asInstanceOf[T]
    case _: Int  => Random.nextInt(100).asInstanceOf[T]
    case _: Integer  => Random.nextInt(100).asInstanceOf[T]
    case _: Option[Integer]  => Some(Random.nextInt(100)).asInstanceOf[T]
    case _: Option[Int] => Some(Random.nextInt(100)).asInstanceOf[T]
    case _: Boolean => Random.nextBoolean().asInstanceOf[T]
    case _: Option[Boolean] => Some(Random.nextBoolean()).asInstanceOf[T]
    case _: Double => Random.nextDouble().asInstanceOf[T]
    case _: Option[Double] => Some(Random.nextDouble()).asInstanceOf[T]
    case _: Float => Random.nextFloat().asInstanceOf[T]
    case _: Option[Float] => Some(Random.nextFloat()).asInstanceOf[T]
    case _: Long => Random.nextLong().asInstanceOf[T]
    case _: Option[Long] => Some(Random.nextLong()).asInstanceOf[T]
    case _ =>
      val typeName = summonInline[scala.reflect.ClassTag[T]].runtimeClass.getSimpleName
      println(s"⚠️ Unsupported type: $typeName")
      throw new UnsupportedOperationException(s"Cannot generate random value for type $typeName")

trait RandomGenerator[T<:Product]:
  def generate: T


object RandomGenerator:
  transparent inline given derived[T <: Product](using m: Mirror.ProductOf[T], tag: ClassTag[T]): RandomGenerator[T] = {
    new RandomGenerator[T] {
     

      def generate: T = {
        val labels = constValueTuple[m.MirroredElemLabels].toIArray.toList.map(_.toString)
        val zipped = labels.zip(getClassesFieldType)
        val extractedValues = zipped.map {
          case (label, "String") => Random.alphanumeric.take(10).mkString
          case (label, "Option[String]") => Some(Random.alphanumeric.take(10).mkString)
          case (label, "Int") => Random.nextInt(100)
          case (label, "Boolean") => Random.nextBoolean()
          case (label, "Double") => Random.nextDouble()
          case (label, "Float") => Random.nextFloat()
          case (label, "Long") => Random.nextLong()
          case _ => throw new IllegalArgumentException(s"Unsupported field type")
        }

        // Convert to Tuple for Mirror's apply method
        val valuesTuple = Tuple.fromArray(extractedValues.toArray)

        // Use Mirror to instantiate case class
        m.fromProduct(valuesTuple)
      }

      def getClassesFieldType: List[String] = {
        inline m match {
          case m: Mirror.ProductOf[T] => {

            summonFieldTypes[m.MirroredElemTypes]
          }

        }
      }

    }
  }

