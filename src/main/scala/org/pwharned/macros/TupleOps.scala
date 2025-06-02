package org.pwharned.macros
import scala.quoted.*

object TupleMacros:

  // --- Public API ---
  inline def constructTuple[T <: Tuple](inline elems: String*): T =
    ${ constructTupleImpl[T]('elems) }

  def constructTupleImpl[T <: Tuple: Type](elemsExpr: Expr[Seq[String]])(using q: Quotes): Expr[T] =
    elemsExpr match
      case Varargs(elems) =>
        buildTupleExpr[T](elems.toList)
      case _ =>
        q.reflect.report.errorAndAbort("Expected explicit string literals")

  // --- Recursive Tuple Builder ---
  // Builds a tuple expression of type T by recursively processing the list of string expressions.
  def buildTupleExpr[T <: Tuple](strings: List[Expr[String]])(using q: Quotes, tpe: Type[T]): Expr[T] =
    import q.reflect.*
    tpe match
      case '[EmptyTuple] =>
        if strings.nonEmpty then
          report.errorAndAbort("Too many arguments for tuple construction")
        else
          // Cast is safe because T is EmptyTuple.
          '{ EmptyTuple }.asInstanceOf[Expr[T]]
      case '[h *: t] =>
        if strings.isEmpty then
          report.errorAndAbort("Not enough arguments for tuple construction")
        else
          // Convert the head string to the target type h.
          val headExpr: Expr[h] = convertExpr[h](strings.head)
          // Recursively build the tail of the tuple.
          val tailExpr: Expr[t] = buildTupleExpr[t](strings.tail)
          // Cast is safe because T is h *: t.
          '{ $headExpr *: $tailExpr }.asInstanceOf[Expr[T]]
      case _ =>
        report.errorAndAbort("Provided type is not a tuple")

  // --- Conversion Helper ---
  // Converts a string literal into a value of type A.
  // Supports conversions to Int, String, and Boolean.
  def convertExpr[A: Type](strExpr: Expr[String])(using q: Quotes): Expr[A] =
    import q.reflect.*
    val targetType = TypeRepr.of[A]
    strExpr match
      case Expr(s: String) =>
        if targetType =:= TypeRepr.of[Int] then
          s.toIntOption match
            case Some(i) => Expr(i).asInstanceOf[Expr[A]]
            case None    => report.errorAndAbort(s"Cannot convert '$s' to Int")
        else if targetType =:= TypeRepr.of[String] then
          Expr(s).asInstanceOf[Expr[A]]
        else if targetType =:= TypeRepr.of[Boolean] then
          s.toLowerCase match
            case "true"  => '{ true  }.asInstanceOf[Expr[A]]
            case "false" => '{ false }.asInstanceOf[Expr[A]]
            case _       => report.errorAndAbort(s"Cannot convert '$s' to Boolean")
        else
          report.errorAndAbort(s"Unsupported conversion from String to type ${targetType.show}")
      case _ =>
        report.errorAndAbort("Expected a string literal")
