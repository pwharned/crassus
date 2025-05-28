package org.pwharned.macros

import scala.annotation.StaticAnnotation
import scala.quoted.*

class PrimaryKey extends StaticAnnotation
// Define the annotation

object PrimaryKeyExtractor:
  inline def getPrimaryKey[T]: Seq[String] = ${ getPrimaryKeyImpl[T] }

  def getPrimaryKeyImpl[T](using Quotes, Type[T]): Expr[Seq[String]] =
    import quotes.reflect.*
    val sym = TypeRepr.of[T].typeSymbol
    val fields = sym.primaryConstructor.paramSymss.flatten
    fields.foreach { field =>
      val fieldAnnotations = field.annotations.map(_.show)
      println(s"Field: ${field.name}, Annotations: ${fieldAnnotations.mkString(", ")}")
    }

    val primaryKeys = fields.collect {
      case f if f.annotations.exists(a => a.tpe.show.contains( "PrimaryKey")) => f.name
    }

    Expr(primaryKeys)

  inline def getPrimaryKeyTypes[T]: List[Type[?]] = ${ getPrimaryKeyTypesImpl[T] }
  
  def getPrimaryKeyTypesImpl[T](using Quotes, Type[T]): Expr[List[Type[?]]] =
    import quotes.reflect.*
  
    val sym = TypeRepr.of[T].typeSymbol
    val fields = sym.primaryConstructor.paramSymss.flatten
  
    val primaryKeyTypes:List[Expr[Type[?]]] = fields.collect {
      case f if f.annotations.exists(_.tpe =:= TypeRepr.of[PrimaryKey]) =>
        f.typeRef.asType match
          case '[t] =>
            given ToExpr[Type[t]] with { def apply(x: Type[t])(using Quotes): Expr[Type[t]] = '{ x } }
            Expr(Type.of[t])
    }
  
    Expr.ofList(primaryKeyTypes)
