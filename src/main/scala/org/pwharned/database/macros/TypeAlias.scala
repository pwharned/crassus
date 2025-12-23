package org.pwharned.database.macros

// src/main/scala/Macros.scala
import scala.quoted.*

import scala.quoted.*

object Macros:
  transparent inline def aliasNameOf[T]: String = ${ aliasNameOfImpl[T] }

  private def aliasNameOfImpl[T: Type](using q: Quotes): Expr[String] =
    import q.reflect.*

    def nameFromTr(t: TypeRepr): String =
      // Prefer the symbol name if present (this preserves alias names)
      val sym = t.typeSymbol
      if sym != Symbol.noSymbol && sym.name.nonEmpty then sym.name
      else
        // Handle common trees that don't expose a direct symbol name
        t match
          case AppliedType(tycon, args) =>
            // If the applied constructor itself has a symbol, return it,
            // otherwise try the first type argument
            val csym = tycon.typeSymbol
            if csym != Symbol.noSymbol && csym.name.nonEmpty then csym.name
            else if args.nonEmpty then nameFromTr(args.head)
            else
              tycon.show.split(Array('.', '$')).lastOption.getOrElse(tycon.show)
          case TypeRef(_, name)        => name
          case TermRef(_, name)        => name
          case AnnotatedType(under, _) => nameFromTr(under)
          case ByNameType(under)       => nameFromTr(under)
          case other                   =>
            // fallback: last segment of the shown type
            other.show.split(Array('.', '$')).lastOption.getOrElse(other.show)

    // Start from the top-level type for T, if it's an applied type take the first arg
    val root = TypeRepr.of[T]
    val result =
      root match
        case AppliedType(_, args) if args.nonEmpty => nameFromTr(args.head)
        case other                                 => nameFromTr(other)

    Expr(result)
