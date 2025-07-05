package org.pwharned.macros

import scala.quoted._


transparent inline def typeName[T]: String = ${ typeNameImpl[T] }

def typeNameImpl[T: Type](using Quotes): Expr[String] =
  import quotes.reflect._
  Expr(TypeRepr.of[T].show)


transparent inline def simpleTypeName[T]: String = ${ simpleTypeNameImpl[T] }

def simpleTypeNameImpl[T: Type](using q: Quotes): Expr[String] = {
  import q.reflect.*

  /**
   * Recursively strip away any type constructor 
   * (e.g. Iterator[…], List[…], Either[_,…], etc.)
   * until we hit a bare TypeRef, then take its name.
   */
  def simpleName(tp: TypeRepr): String = tp.dealias match
    case AppliedType(_, targs) if targs.nonEmpty =>
      simpleName(targs.head)
    // sometimes you get TermRef for inner classes or modules
    case tref: TypeRef => tref.name
    case tref: TermRef => tref.name
    case other =>
      // fallback to last segment of the printed name
      other.show.split('.').last

  val name = simpleName(TypeRepr.of[T])
  Expr(name)
}