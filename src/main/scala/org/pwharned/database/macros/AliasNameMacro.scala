package org.pwharned.database.macros

// Save this as AliasNameMacro.scala and compile with Scala 3
import scala.quoted.*

object AliasNameMacro:

  inline def aliasNameOf[T]: String = ${ aliasNameOfImpl[T] }

  private def aliasNameOfImpl[T: Type](using q: Quotes): Expr[String] =
    import q.reflect.*

    def nameFrom(tr: TypeRepr): Option[String] =
      // Prefer the symbol on the exact TypeRepr we were given (don't dealias first)
      val sym = tr.typeSymbol
      if sym != Symbol.noSymbol && sym.name.nonEmpty then Some(sym.name)
      else
        tr match
          case AppliedType(_, args) if args.nonEmpty =>
            args.head match
              case tref: TypeRef =>
                val asym = tref.typeSymbol
                if asym != Symbol.noSymbol && asym.name.nonEmpty then
                  Some(asym.name)
                else None
              case other => None
          case TypeRef(_, name) => Some(name)
          case TermRef(_, name) => Some(name)
          case _                => None

    val root = TypeRepr.of[T]
    val aliasOpt = root match
      case AppliedType(_, args) if args.nonEmpty =>
        nameFrom(args.head).orElse(nameFrom(root))
      case _ =>
        nameFrom(root)

    // Emit compile-time info so you can see what's happening during compilation
    report.info(
      s"aliasNameOf macro: original = ${root.show}; dealias = ${root.dealias.show}"
    )

    Expr(aliasOpt.getOrElse("<<no-alias-found>>"))
