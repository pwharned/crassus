package org.pwharned.experiments

import scala.quoted.*

sealed trait Being
case object Dog extends Being
case object Cat extends Being
case object Cow extends Being

class Animal[A <: Being](val name: String, val entity: A)

object AnimalMacro:

  /**
   * 1) Immediate dispatch: inline key + nested-match tree at the call site
   */
  inline def dispatchAnimal(
                             inline key:    String,
                             inline animals: Animal[? <: Being]*
                           ): Animal[? <: Being] =
    ${ dispatchAnimalPathImpl('key, 'animals) }


  /**
   * 2) Deferred dispatch: build a String => Animal function once,
   *    then call it at runtime for each incoming path
   */
  inline def dispatchAnimalPathFn(
                                   inline animals: Animal[? <: Being]*
                                 ): String => Animal[? <: Being] =
    ${ dispatchAnimalPathFnImpl('animals) }


  // ------------------------------------------------------------
  // Implementation of the _immediate_ dispatch macro
  // ------------------------------------------------------------
  private def dispatchAnimalPathImpl(
                                      keyExpr:     Expr[String],
                                      animalsExpr: Expr[Seq[Animal[? <: Being]]]
                                    )(using Quotes): Expr[Animal[? <: Being]] =
    import quotes.reflect.*

    // 1. Unpack the repeated args into (Expr[Animal], List[pathSegments])
    val data: Seq[(Expr[Animal[? <: Being]], List[String])] =
      animalsExpr match
        case Varargs(es) =>
          es.map { ae =>
            ae.asTerm match
              case Apply(fun @ (Select(New(_), "<init>") |
                                TypeApply(Select(New(_), "<init>"), _)),
              List(Literal(StringConstant(n)), _)) =>
                (ae, n.stripPrefix("/").split("/").toList)
              case other =>
                report.errorAndAbort(
                  s"Each animal must be `new Animal(\"/a/b/...\", entity)`, but got:\n  ${other.show}"
                )
          }
        case _ =>
          report.errorAndAbort("animals must be provided as repeated args")

    /** is this segment a dynamic placeholder? */
    def isDyn(seg: String): Boolean =
      seg.startsWith("{") && seg.endsWith("}")

    /** Recursively build a nested `Match` tree on segment `depth` */
    def buildMatch(
                    depth:   Int,
                    entries: Seq[(Expr[Animal[? <: Being]], List[String])]
                  ): Term =

      // scrutinee for this level: extract segment #depth
      val segTerm: Term =
        '{
          val arr = $keyExpr.stripPrefix("/").split("/")
          arr.apply(${ Expr(depth) })
        }.asTerm

      // split into exact vs dynamic branches at this depth
      val (dynEntries, statEntries) = entries.partition { case (_, segs) =>
        isDyn(segs(depth))
      }

      // group exact entries by the literal segment
      val statGroups: Map[String, Seq[(Expr[Animal[? <: Being]], List[String])]] =
        statEntries.groupBy(_._2(depth))

      // one CaseDef per exact segment
      val statCases: List[CaseDef] = statGroups.toList.map { (seg, es) =>
        val body: Term =
          if es.forall(_._2.size == depth + 1) then
            // leaf: return the Animal
            es.head._1.asTerm
          else
            // deeper: recurse to the next segment
            buildMatch(depth + 1, es)
        CaseDef(Literal(StringConstant(seg)), None, body)
      }

      // wildcard: either dive into dynamic placeholders or error
      val wildcardBody: Term =
        if dynEntries.nonEmpty then
          if dynEntries.forall(_._2.size == depth + 1) then
            dynEntries.head._1.asTerm
          else
            buildMatch(depth + 1, dynEntries)
        else
          '{ throw new MatchError($keyExpr) }.asTerm

      val wildcardCase = CaseDef(Wildcard(), None, wildcardBody)

      Match(segTerm, statCases :+ wildcardCase)

    // kick off at depth = 0
    buildMatch(0, data).asExprOf[Animal[? <: Being]]


  // ------------------------------------------------------------
  // Implementation of the _deferred_ dispatch macro
  // ------------------------------------------------------------
  private def dispatchAnimalPathFnImpl(
                                        animalsExpr: Expr[Seq[Animal[? <: Being]]]
                                      )(using Quotes): Expr[String => Animal[? <: Being]] =
    import quotes.reflect.*

    // 1. Unpack the repeated args
    val data: Seq[(Expr[Animal[? <: Being]], List[String])] =
      animalsExpr match
        case Varargs(es) =>
          es.map { ae =>
            ae.asTerm match
              case Apply(fun @ (Select(New(_), "<init>") |
                                TypeApply(Select(New(_), "<init>"), _)),
              List(Literal(StringConstant(n)), _)) =>
                (ae, n.stripPrefix("/").split("/").toList)
              case other =>
                report.errorAndAbort(
                  s"Each animal must be `new Animal(\"/a/b/...\", entity)`, but got:\n  ${other.show}"
                )
          }
        case _ =>
          report.errorAndAbort("animals must be provided as repeated args")

    def isDyn(seg: String): Boolean =
      seg.startsWith("{") && seg.endsWith("}")

    /**
     * Recursively build a nested `Match` on segment `depth`, using the
     * provided `keyExpr` as the path input.
     */
    def buildMatch(
                    depth:   Int,
                    entries: Seq[(Expr[Animal[? <: Being]], List[String])],
                    keyExpr: Expr[String]
                  ): Term =

      // scrutinee: segment at `depth` from the runtime `key`
      val segTerm: Term =
        '{
          val arr = $keyExpr.stripPrefix("/").split("/")
          arr.apply(${ Expr(depth) })
        }.asTerm

      // partition exact vs dyn
      val (dynEntries, statEntries) = entries.partition { case (_, segs) =>
        isDyn(segs(depth))
      }

      val statGroups = statEntries.groupBy(_._2(depth))

      val statCases: List[CaseDef] = statGroups.toList.map { (seg, es) =>
        val body =
          if es.forall(_._2.size == depth + 1) then
            es.head._1.asTerm
          else
            buildMatch(depth + 1, es, keyExpr)
        CaseDef(Literal(StringConstant(seg)), None, body)
      }

      val wildcardBody =
        if dynEntries.nonEmpty then
          if dynEntries.forall(_._2.size == depth + 1) then
            dynEntries.head._1.asTerm
          else
            buildMatch(depth + 1, dynEntries, keyExpr)
        else
          '{ throw new MatchError($keyExpr) }.asTerm

      val wildcardCase = CaseDef(Wildcard(), None, wildcardBody)

      Match(segTerm, statCases :+ wildcardCase)

    // 2. Emit a lambda (key: String) => nestedMatch(key)
    '{
      (key: String) =>
        ${
          // here’s the only change: convert the Term into an Expr
          buildMatch(0, data, '{ key }).asExprOf[Animal[? <: Being]]
        }
    }
end AnimalMacro
