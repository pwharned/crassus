package org.pwharned

import org.pwharned.sql.derive.{SqlInsert, TableOrganizationMacro}
import generated.*
import org.pwharned.sql.HKD.New
import org.pwharned.sql.dialect.{Db2Dialect, SqlDialect}
@main
def main() : Unit = {
  case class naics_embeddings[F[_]]()
  given dial: SqlDialect = Db2Dialect
  val sqlInsert: SqlInsert[New[naics_embeddings]] = summon[SqlInsert[New[naics_embeddings]]]
  val sql = sqlInsert.sql(naics_embeddings())
  println(Db2Dialect.insertReturning[New[naics_embeddings]]("test"))
  println(sql)
  println(TableOrganizationMacro.tableInfo[New[naics_embeddings]])

}