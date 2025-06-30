package org.pwharned.macros

import scala.quoted._


transparent inline def typeName[T]: String = ${ typeNameImpl[T] }

def typeNameImpl[T: Type](using Quotes): Expr[String] =
  import quotes.reflect._
  Expr(TypeRepr.of[T].show)