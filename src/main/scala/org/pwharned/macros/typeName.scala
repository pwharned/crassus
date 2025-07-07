package org.pwharned.macros

import scala.quoted._


inline def typeToString[T]: String =
  ${ _s[T] }
private def _s[T: Type](using Quotes) =
  Expr(Type.show[T])

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







inline def extractEntityType[T]: String = ${ extractEntityTypeImpl[T] }

def extractEntityTypeImpl[T: Type](using Quotes): Expr[String] = {
  import quotes.reflect.*

  def findEntities(tpe0: TypeRepr): Option[String] = {
    val tpe = tpe0.dealias
    tpe match {

      // direct hit
      case AppliedType(tycon, List(arg))
        if tycon.typeSymbol.fullName == "generated.entities" =>
        Some(s"generated.entities[${arg.show}]")

      // recurse into applied-type args
      case AppliedType(_, args) =>
        args.iterator.flatMap(findEntities).nextOption()

      // unpack MatchType(scrutinee, bound, cases: List[(pattern, result)])
      // 3. MatchType: scrutinee, bound, then each case’s .to
      case mt: MatchType =>
        findEntities(mt.scrutinee)
          .orElse(findEntities(mt.bound))
        //  .orElse {
        //    mt.cases.iterator
    //          .flatMap(tc => findEntities(tc.to))
       //       .nextOption()
     //     }


      // unpack type lambda (params, bounds, body)
      case TypeLambda(_, _, body) =>
        findEntities(body)

      // polymorphic methods
      case mt: MethodType =>
        findEntities(mt.resType)

      // by-name, refinements, intersections…
      case by: ByNameType =>
        findEntities(by.underlying)
      case Refinement(parent, _, info) =>
        findEntities(parent).orElse(findEntities(info))
      case AndType(left, right) =>
        findEntities(left).orElse(findEntities(right))

      case _ =>
        None
    }
  }

  val result = findEntities(TypeRepr.of[T])
    .getOrElse(s"${TypeRepr.of[T].show}")
  Expr(result)
}
