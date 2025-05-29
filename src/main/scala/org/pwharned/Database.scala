package org.pwharned

import org.pwharned.macros.{Db2TypeMapper, DbTypeMapper}
import org.pwharned.macros.{Db2TypeMapper, DbTypeMapper, RandomGenerator, classFieldTypes, createTable, createTableAsync, insertAsync, query, select, update, seraialize, streamQuery}

object Database:
  given db:DbTypeMapper = Db2TypeMapper
  // Example usage within your streaming query
  def getDbConnection(): java.sql.Connection = {
    val url = "jdbc:db2://localhost:50000/BLUDB"
    val user = "db2inst1"
    val password = "password"
  
    Class.forName("com.ibm.db2.jcc.DB2Driver") // Load DB2 JDBC driver
    java.sql.DriverManager.getConnection(url, user, password)
  }