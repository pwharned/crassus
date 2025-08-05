package org.pwharned

import org.pwharned.SqlString.{Parser, stringInsensitive}

sealed trait SqlDataType extends Parse {
  def sqlNames: Seq[String]
  def parse: Parser[SqlDataType]
  def scalaType: String
}
case object SqlInteger extends SqlDataType  {
  val sqlNames: Seq[String] = Seq("INTEGER", "INT")
  def scalaType: String = "Int"
  def parse: Parser[SqlDataType] =
    (stringInsensitive("INTEGER") or stringInsensitive("INT")).map(_ => this)
}
case object SqlUuid extends  SqlDataType {
  val sqlNames: Seq[String] = Seq( "UUID")
  def scalaType: String = "java.util.UUID"
  def parse: Parser[SqlDataType] = stringInsensitive("UUID").map(x => this)

}
case object SqlVector extends SqlDataType  {
  val sqlNames: Seq[String] = Seq( "Vector")
  def scalaType: String = "Vector[Float]"


  val vectorParser = for {
    _ <- whitespace
    initial <- stringInsensitive("ibm_extension.").optional
    ch <- stringInsensitive("vector")
    _ <- whitespace
    open <- char('(')
    _ <- whitespace
    number <- numeric.many
    _ <- whitespace
    close <- char(')')
  } yield ch + open + number.mkString + close
  def parse: Parser[SqlDataType] =
    ( vectorParser).map(_ => this)
}
case object SqlString extends SqlDataType  {
  val sqlNames: Seq[String] = Seq( "TEXT")
  def scalaType: String = "String"


  val varcharparser = for {
    _ <- whitespace
    ch <- stringInsensitive("character")
    _ <- whitespace
    va <- stringInsensitive("varying")
    _ <- whitespace
    open <- char('(')
    _ <- whitespace
    number <- numeric.many
    _ <- whitespace
    close <- char(')')
  } yield va + open + number.mkString + close
  def parse: Parser[SqlDataType] =
    ( stringInsensitive("TEXT") or varcharparser).map(_ => this)
}

case object SqlTextArray extends SqlDataType  {
  val sqlNames: Seq[String] = Seq( "TEXT[]")
  def scalaType: String = "List[String]"

  def parse: Parser[SqlDataType] =
    stringInsensitive("TEXT[]") .map(x => this)
}


case object SqlBoolean extends SqlDataType {
  val sqlNames: Seq[String] = Seq("BOOLEAN")
  def scalaType: String = "Boolean"


  def parse: Parser[SqlDataType] = stringInsensitive("BOOLEAN").map(_ => this)
}
case object SqlFloat extends SqlDataType {
  val sqlNames: Seq[String] = Seq("DOUBLE PRECISION", "FLOAT")
  def scalaType: String = "Float"


  def parse: Parser[SqlDataType] =
    (stringInsensitive("DOUBLE PRECISION") or stringInsensitive("FLOAT")).map(_ => this)
}
case object SqlDate extends SqlDataType  {
  val sqlNames = Seq("DATE")
  def scalaType: String = "Date"


  def parse: Parser[SqlDataType] = stringInsensitive("DATE").map(_ => this)
}
case object SqlTimestamp extends SqlDataType {
  val sqlNames = Seq("TIMESTAMP WITH TIME ZONE", "TIMESTAMP")
  def scalaType: String = "String"

  def parse: Parser[SqlDataType] =
    (stringInsensitive("TIMESTAMP WITH TIME ZONE") or stringInsensitive("TIMESTAMP"))
      .map(_ => this)
}

object SqlDataType extends Parse {
  val values: List[SqlDataType] = List(
    SqlInteger, SqlString, SqlBoolean, SqlFloat, SqlDate, SqlTimestamp, SqlTextArray
  )


  // Helper method to find a DataType by its SQL name
  def fromString(s: String): Option[SqlDataType] =
    values.find(_.sqlNames.exists(sqlName => sqlName.equalsIgnoreCase(s)))
}