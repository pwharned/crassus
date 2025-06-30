package org.pwharned.database


import scala.compiletime.*
import scala.deriving.*
import scala.quoted.*

trait SqlInsert[T]:
  def insertStatement: String
  def insertStatementNoReturn: String
  def bindValues(obj: T): Seq[Any]

object SqlInsert:
  inline given derived[T <: Product](using m: Mirror.ProductOf[T]): SqlInsert[T] =
    ${ sqlInsertImpl[T]('{ m }) }

def sqlInsertImpl[T: Type](mExpr: Expr[Mirror.ProductOf[T]])(using q: Quotes): Expr[SqlInsert[T]] = {
  import q.reflect.*

  // Obtain the type representation of T:
  val tpe: TypeRepr = TypeRepr.of[T]
  val classSymbol: Symbol = tpe.typeSymbol
  val tableName: String = classSymbol.name

  // Retrieve the list of case class fields:
  val fields: List[Symbol] = classSymbol.caseFields

  // Helper to check if a field is of type PrimaryKey:
  def isPkey(sym: Symbol): Boolean =
    tpe.memberType(sym).dealias match {
      case AppliedType(t, _ :: Nil) =>
        report.info(s"Compile-time debug: Field '${t.typeSymbol.name}' ")
        t.typeSymbol.name.contains( "PrimaryKey")
      case _ => false
    }

  def isPrimaryKeyFieldString(sym: Symbol): Boolean =
    tpe.memberType(sym).dealias.show match {
      case x: String if x.contains("PrimaryKey") =>  true
      case _ => false
    }
  // Exclude PrimaryKey fields:
  val nonKeyFields: List[Symbol] = fields.filterNot(isPrimaryKeyFieldString)
  
  val completeFieldNameString: String = fields.map(_.name).mkString(",")
  val fieldNames: List[String] = nonKeyFields.map(_.name)
  val fieldNamesStr: String = fieldNames.mkString(",")
  val placeholdersStr: String = fieldNames.map(_ => "?").mkString(",")

  // Build the SQL statements:
  val stmt: String =
    s"select $completeFieldNameString from final table (INSERT INTO $tableName ($fieldNamesStr) VALUES ($placeholdersStr));"
  val stmtNoReturn: String =
    s"INSERT INTO $tableName ($fieldNamesStr) VALUES ($placeholdersStr);"

  // Find the indices corresponding to non-key fields in the Product:
  val nonKeyIndices: List[Int] =
    fields.zipWithIndex.collect { case (sym, idx) if !isPrimaryKeyFieldString(sym) => idx }
    
  val test = nonKeyIndices.length

  val bindValuesExpr: Expr[T => Seq[Any]] = '{
    (obj: T) =>
      val p = obj.asInstanceOf[Product]
      ${ Expr.ofList(nonKeyIndices.map { idx =>
        val idxExpr = Expr(idx)
        '{ p.productElement(${ idxExpr }) match
          case Some(v) => v
          case None => null
          case other => other
        }
      })
      }
  }


  '{
    new SqlInsert[T]:
      def insertStatement: String =
      {  ${Expr(stmt)} }
      def insertStatementNoReturn: String = ${Expr(stmtNoReturn)}
      def bindValues(obj: T): Seq[Any] =
      { $bindValuesExpr(obj) }
  }
}
