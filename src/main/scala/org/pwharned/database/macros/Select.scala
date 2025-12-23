package org.pwharned.database.macros
import scala.quoted.*

trait Select[T] {
  def select: String
}

object Select:
  transparent inline given derived[T]: Select[T] = ${ selectImpl[T] }

  def selectImpl[T: Type](using
      q: scala.quoted.Quotes
  ): scala.quoted.Expr[Select[T]] =
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

    def constStringOf(t: TypeRepr): Option[String] =
      t match
        case ConstantType(StringConstant(s)) => Some(s)
        case _                               => None

    val tpe = TypeRepr.of[T].dealias

    def extractNameTuple(tr: TypeRepr): Option[TypeRepr] =
      tr match
        case AppliedType(_, args) if args.nonEmpty => Some(args.head)
        case _                                     => None

    val namesListOpt: Option[List[String]] =
      extractNameTuple(tpe).flatMap { namesTuple =>
        def tupleElems(tp: TypeRepr): List[TypeRepr] = tp match
          case AppliedType(tpSym, args)
              if tpSym.typeSymbol.fullName == "scala.Tuple" || tpSym.typeSymbol.fullName == "scala.EmptyTuple" =>
            args.flatMap(
              tupleElems
            ) // defensive; usually scala.Tuple constructors vary by arity

        val nameArgs = namesTuple.dealias.typeArgs
        val strs = nameArgs.flatMap(constStringOf)
        if strs.length == nameArgs.length then Some(strs) else None
      }
    val sqlExpr = Expr(
      s"select ${namesListOpt.getOrElse(List("*")).mkString(",")} from  ${aliasOpt.getOrElse("<<no-alias-found>>")};"
    )

    val emitted = '{
      val __selectInstance: org.pwharned.database.macros.Select[T] =
        new org.pwharned.database.macros.Select[T]:
          val select: String = $sqlExpr
      __selectInstance
    }
    report.info(
      s"select ${namesListOpt.getOrElse(List("*")).mkString(",")} from ${aliasOpt.getOrElse("<<no-alias-found>>")}"
    )
    emitted
    // Expr(s"select ${namesListOpt.getOrElse(List("*")).mkString(",") } from $result ;")
