package org.pwharned.generator
import org.pwharned
import org.pwharned.Parse.createTableParser

object CaseClassGenerator {
  def generateCaseClasses(): String = {
    // val lines = Source.fromFile(inputFile).getLines().toList
    createTableParser("create table user(id integer, name text)") match {
      case Left(value) => throw new Exception(value.message)
      case Right(value) =>
        s"""package generated
           |case class ${value._1.name} (${value._1.columns.map(x => x.toField).mkString(",") })""".stripMargin
    }
  }
}





