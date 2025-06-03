package org.pwharned.generator
import org.pwharned.SQLParser
import org.pwharned.SQLParser.ColumnOps

//case class User[F[_]](id: F[PrimaryKey[Int]], name: F[String])
object CaseClassGenerator  {
  def generateCaseClasses(): String = {
    // val lines = Source.fromFile(inputFile).getLines().toList
    SQLParser.createTableParser("create table user(id int not null primary key generated always as identity, name text not null, test text)") match {
      case Left(value) => throw new Exception(value.message)
      case Right(value) =>
        s"""
           |case class ${value._1.name}[F[_]] (${value._1.columns.map(x => x.toField).mkString(",") })""".stripMargin
    }
  }
}





