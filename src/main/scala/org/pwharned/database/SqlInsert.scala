package org.pwharned.database

import org.pwharned.database.statements.SqlDialect
import scala.deriving.Mirror

trait SqlInsert[T]:
  def insertReturning(obj: T): String

object SqlInsert:

  // --------------------------------------------------------------------------
  // 1) Comment out the old macro‐based implementation
  // --------------------------------------------------------------------------
  // inline given derived[T <: Product](using
  //                                    m: Mirror.ProductOf[T],
  //                                    dialect: SqlDialect
  //                                   ): SqlInsert[T] =
  //   ${ sqlInsertImpl[T]('{ dialect }) }
  //
  // private def sqlInsertImpl[T: Type](
  //                                     dialectExpr: Expr[SqlDialect]
  //                                   )(using q: Quotes): Expr[SqlInsert[T]] = 
  //   … your old macro …
  //
  // --------------------------------------------------------------------------
  // 2) New implementation, purely in terms of `Insertable`
  // --------------------------------------------------------------------------

  inline given derived[T <: Product](using
                                     ins: Insertable[T],
                                     dialect: SqlDialect
                                    ): SqlInsert[T] =
    new SqlInsert[T]:
      def insertReturning(obj: T): String =
        val bareSql = ins.sql(obj)

        val finalSql = dialect.insertReturning(ins.tableName)
        finalSql
