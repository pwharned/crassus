package org.pwharned.generator
import org.pwharned.{Column, SQLParser}
import org.pwharned.SQLParser.{ColumnOps, alterTablePrimaryKeyParser}

import java.io.File
import java.nio.CharBuffer
import scala.io.Source

//case class User[F[_]](id: F[PrimaryKey[Int]], name: F[String])
object CaseClassGenerator  {
  def generateCaseClasses(filePath: String): String = {
    // Get resource as stream from classpath
    val input = Source.fromFile(filePath)("UTF-8")
    //val resourceStream = getClass.getResourceAsStream("/schema.sql")
    //if (resourceStream == null) {
    //  throw new IllegalStateException("Could not find schema.sql in resources")
   /// }
    val file = new File(filePath)
    if (!file.exists() || !file.isFile) {
      throw new IllegalArgumentException(s"Schema file not found at: $filePath")
    }


    val lines = input.getLines().mkString("\n")
    System.out.println("Discovered statements are  :" + lines)
    val statements  = lines.split(";")
    // val lines = Source.fromFile(inputFile).getLines().toList
    val createTableStatements = statements.filter( x=> x.trim.toUpperCase.startsWith("CREATE TABLE")).map(x => SQLParser.createTableParser(x))

    val alterTableStatements = statements.filter( x=> x.trim.toUpperCase.startsWith("ALTER TABLE")).map( x=> SQLParser.alterTablePrimaryKeyParser(x))

    createTableStatements.map {
      case Left(value) => {System.out.println(value)}
      case Right(value) =>
        val alterations = alterTableStatements.filter {
          x => x.isRight
        }.map( x=> x.toOption.get).filter( x=> x._1.table.toUpperCase==value._1.name.toUpperCase)

        val columns = value._1.columns.map( x=> {
          alterations.find(y => y._1.columns.contains(x.name)) match {
            case Some(value) => Column(x.name, x.dataType,x.nullable,Some(true),x.generated_always_as_identity)
            case None => x
          }
        })

        System.out.println(value)
        s"""
           |case class ${value._1.name}[F[_]] (${columns.map(x => x.toField).mkString(",")})""".stripMargin
    }

  }.mkString("\n")
}





