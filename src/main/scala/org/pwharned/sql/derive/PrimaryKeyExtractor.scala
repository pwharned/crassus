package org.pwharned.sql.derive

import org.pwharned.sql.HKD._

import scala.compiletime.*
import scala.deriving.*
import scala.quoted.{Expr, Quotes, Type}

object PrimaryKeyExtractor:


  inline def getPrimaryKey[T <: Product]: Seq[String] =
    ${ getPrimaryKeyImpl[T] }

  private def getPrimaryKeyImpl[T: Type](using q: Quotes): Expr[Seq[String]] = {
    import q.reflect._

    // helper: does `t` anywhere contain PrimaryKey[_] ?
    def containsPK(t: TypeRepr): Boolean =
      t.dealias match
        case AppliedType(tycon, args) =>
          // if this constructor *is* PrimaryKey
          if tycon.typeSymbol.fullName.contains( "PrimaryKey") then true
          else args.exists(containsPK)
        case _ =>
          false

    val tpe = TypeRepr.of[T].dealias
    val sym = tpe.typeSymbol

    val names: Seq[Expr[String]] =
      sym.caseFields
        .filter(fld => containsPK(tpe.memberType(fld)))
        .map(fld => Expr(fld.name))
    val listExpr: Expr[List[String]] = Expr.ofList(names)
    listExpr.asExprOf[Seq[String]]
  }
object TupleKeyExtractor:

  transparent inline def extractPkTuple[T <: Product](orig: T)(using
                                                               m: Mirror.ProductOf[T]
  ): Tuple =
    // 1) grab the compile‐time field names
    val labels: List[String] =
      constValueTuple[m.MirroredElemLabels].toList.map(_.toString)

    // 2) ask your primary‐key extractor for the names we want to keep
    val pkNames: Seq[String] = PrimaryKeyExtractor.getPrimaryKey[T]

    // 3) for each label, if it's in pkNames, pull that index from orig.productElement
    val pkValues: Array[Any] =
      labels
        .zipWithIndex
        .collect { case (lbl, idx) if pkNames.contains(lbl) =>
          orig.productElement(idx)
        }
        .toArray

    // 4) turn the Array back into a Tuple
    Tuple.fromArray(pkValues)




