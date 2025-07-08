package org.pwharned.database


import org.pwharned.database.statements.SqlDialect

import scala.compiletime.*
import scala.deriving.*
import scala.quoted.*

trait SqlInsert[T]:
  def insertReturning(obj: T): (String, Seq[Any])


object SqlInsert:

  inline given derived[T <: Product](using
                                     m: Mirror.ProductOf[T],
                                     dialect: SqlDialect
                                    ): SqlInsert[T] =
    ${ sqlInsertImpl[T]('{ dialect }) }


  private def sqlInsertImpl[T: Type](
                                      dialectExpr: Expr[SqlDialect]
                                    )(using q: Quotes): Expr[SqlInsert[T]] =
    import q.reflect.*

    // reflect on T
    val tpe       = TypeRepr.of[T]
    val sym       = tpe.typeSymbol
    val tableName = sym.name

    // gather fields + index
    val fieldsWithIdx: List[(Symbol, Int)] =
      sym.caseFields.zipWithIndex.toList

    // find your HKD marker (Nullable[_] alias)
    val nullableSym =
      TypeRepr.of[org.pwharned.database.HKD.Nullable[?]].typeSymbol
    val pkey =
      TypeRepr.of[org.pwharned.database.HKD.PrimaryKey[?]].typeSymbol
    // partition into required vs optional

    val (opt, req) = fieldsWithIdx.filter{
      case (f,_) =>       tpe.memberType(f).dealias.show match
        case x: String if x.contains("PrimaryKey") => false
        case _  => true
    } .partition { case (f, _) =>
      tpe.memberType(f).dealias.show match
        case x: String if x.contains("Nullable")  | x.contains("Default") => true
        //case AppliedType(con, _) if con.typeSymbol == nullableSym => true
        case _                                                    => false
    }

    // helper to lift a List[(String,Int)] into an Expr
    def liftList(xs: List[(String, Int)]): Expr[List[(String, Int)]] =
      Expr.ofList(xs.map { case (name, idx) =>
        '{ (${Expr(name)}, ${Expr(idx)}) }
      })

    val reqListExpr = liftList(req.map((f, i) => (f.name, i)))
    val optListExpr = liftList(opt.map((f, i) => (f.name, i)))

    // finally build the class
    '{
      new SqlInsert[T]:
        // step 1: build the column‐names and values
        private def build(obj: T) = {
          val p = obj.asInstanceOf[Product]
          val cols = scala.collection.mutable.ListBuffer.empty[String]
          val values = scala.collection.mutable.ListBuffer.empty[Any]

          // required always
          val reqFields = $reqListExpr
          reqFields.foreach { case (col, idx) =>
            p.productElement(idx) match
            case None =>      throw new IllegalArgumentException(
              s"Field $col is required but got None"
            )
            case Some(v) =>
              cols += col
              values += v
            case other =>
              cols += col
              values += other
          }

          // optional only when Some
          val optFields = $optListExpr
          optFields.foreach { case (col, idx) =>
            p.productElement(idx) match
              case None => () // skip null
              case Some(v) =>
                cols += col
                values += v
              case other =>
                throw new IllegalArgumentException(
                  s"Field $col is marked Nullable or Default but got $other"
                )
          }

          (cols.toList, values.toList)
        }

        override def insertReturning(obj: T): (String, Seq[Any]) =
          val (cols, vals) = build(obj)
          val sql = $dialectExpr.insertReturning(${ Expr(tableName) }, cols)
          (sql, vals)

    }