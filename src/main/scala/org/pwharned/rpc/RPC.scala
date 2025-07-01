package org.pwharned.rpc

import org.pwharned.rpc.{RpcMethodSchema, RpcParam}

import scala.quoted.*


import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonInline}

type Params  = Int | String | Float
transparent inline def listToTuple[T <: Tuple](xs: List[Params], idx: Int = 0): T =
  inline erasedValue[T] match
    case _: EmptyTuple => EmptyTuple.asInstanceOf
    case _: (h *: t) =>
      val head = xs(idx).asInstanceOf[h]
      val tail = listToTuple[t](xs, idx + 1)
      (head *: tail).asInstanceOf[T]

// the refactored listToCaseClass
inline def listToCaseClass[P <: Product](xs: List[Params]): P =
  inline summonInline[Mirror.ProductOf[P]] match
    // this `m` is matched at _compile_ time, so
    // m.MirroredElemTypes is a literal tuple type
    case m: Mirror.ProductOf[P] =>
      // now T = m.MirroredElemTypes is fully known
      val tup = listToTuple[m.MirroredElemTypes](xs)
      m.fromProduct(tup)


case class Type(`type`: String)
inline def rpcMethodDescriptor[F](inline f: F): RpcMethodSchema =
  ${ rpcMethodDescriptorImpl('f) }

def rpcMethodDescriptorImpl(fExpr: Expr[Any])(using q: Quotes): Expr[RpcMethodSchema] = {
  import q.reflect.*

  // 1) Strip away Inlineds, Blocks, DefDefs etc. until we get to the "real" term
  @annotation.tailrec
  def strip(term: Term): Term = term match {
    // discard top‐level inlines
    case Inlined(_, _, inner) => strip(inner)

    // discard synthetic block around a single defdef + closure for η-expansion
    case Block(List(DefDef(_, _, _, Some(rhs))), _) => strip(rhs)

    case other => other
  }

  val core: Term = strip(fExpr.asTerm)

  // 2) Match on the core to find receiver, methodName and argument symbols
  val (receiver: Term, methodName: String) = core match {
    // the common case: svc.foo(arg1,arg2…)
    case Apply(Select(rcv, name), args) =>
      // no need to check args here—just pull name & receiver
      (rcv, name)

    // a zero‐arg def: svc.ping  (no Apply)
    case Select(rcv, name) =>
      (rcv, name)

    // you could also catch a direct Lambda if the compiler didn't inline it
    case Lambda(params, body) =>
      body match {
        case Apply(Select(rcv, name), args)
          if args.map(_.symbol) == params.map(_.symbol) =>
          (rcv, name)
        case other =>
          report.errorAndAbort(s"Unexpected lambda body: ${other.show}")
      }

    case other =>
      report.errorAndAbort(s"Unsupported form: ${other.show}")
  }

  // 3) Look up the method symbol in the receiver’s class
  def findMethod(rcvTpe: TypeRepr, name: String): Symbol = {
    val cls = rcvTpe.classSymbol.getOrElse(
      report.errorAndAbort(s"Cannot find class symbol for $rcvTpe")
    )
    cls.memberMethods
      .find(_.name == name)
      .getOrElse(report.errorAndAbort(s"No method `$name` in ${cls.name}"))
  }

  val methodSym = findMethod(receiver.tpe, methodName)

  // 4) Extract (paramName, paramType)
  val params: List[(String, TypeRepr)] = methodSym.paramSymss.flatten.map { p =>
    val vd = p.tree match {
      case vd: ValDef => vd
      case _ => report.errorAndAbort(s"Param ${p.name} not a ValDef")
    }
    p.name -> vd.tpt.tpe
  }

  // 5) Extract return type
  val returnTpe: TypeRepr = methodSym.tree match {
    case dd: DefDef => dd.returnTpt.tpe
    case other => report.errorAndAbort(s"Expected DefDef, got ${other.show}")
  }

  // 6) A tiny JSON‐Schema helper (expand as you wish)
  def schemaOf(t: TypeRepr, name: String): RpcParam = t.dealias match {
    case t if t =:= TypeRepr.of[Int] => RpcParam(name = name, `type` = "integer")
    case t if t =:= TypeRepr.of[String] => RpcParam(name = name, `type` = "String")
    case t if t.typeSymbol.fullName == "scala.Option" =>
      val arg = t.typeArgs.head
      RpcParam(name = name, `type` = schemaOf(arg, name = name).`type`, required = false)
    case t if t.typeSymbol.flags.is(Flags.Case) =>
      val fs = t.typeSymbol.primaryConstructor.paramSymss.head
      val props = fs.map { f =>
        val fvd = f.tree.asInstanceOf[ValDef]
        RpcParam(name = name, `type` = schemaOf(fvd.tpt.tpe, name = name).`type`)
      }
      RpcParam(name = name, `type` = "object", properties = Some(props))

    case other =>
      report.errorAndAbort(s"Cannot derive schema for ${other.show}")
  }

  val paramsJson = params
    .map { case (n, tp) => {
      schemaOf(tp, n)
    }
    }
  val resultJson = schemaOf(returnTpe, "result")
  val result = RpcMethodSchema(methodName, paramsJson, resultJson.`type`)
  Expr(result)

}


import scala.quoted.{ToExpr, Quotes, Expr}

given ToExpr[RpcParam] with
  def apply(p: RpcParam)(using Quotes): Expr[RpcParam] =
    '{ RpcParam(
      name       = ${Expr(p.name)},
      `type`     = ${Expr(p.`type`)},
      required   = ${Expr(p.required)},
      properties = ${Expr(p.properties.map(ps => (ps))) }
    )}

given ToExpr[RpcMethodSchema] with
  def apply(m: RpcMethodSchema)(using Quotes): Expr[RpcMethodSchema] =
    '{ RpcMethodSchema(
      method = ${Expr(m.method)},
      params = ${Expr.ofList(m.params.map(summon[ToExpr[RpcParam]].apply))},
      result = ${Expr(m.result)}
    )
    }
