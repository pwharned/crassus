package org.pwharned

object SQLParser extends Parse {

  implicit class ColumnOps(column: Column) {
    def toField: String = {
      val typeStr = column.nullable.getOrElse(true) match {
        case true => s"Option[${column.dataType}]"
        case false => column.dataType
      }

      // Add annotation for primary key fields
      column.primary_key match {
        case Some(true) => s" ${column.name}: PrimaryKey[$typeStr]"
        case _ => s"${column.name}: $typeStr"
      }
    }
  }

  val createTableParser: Parser[Table] =
    for {
      _ <- stringInsensitive("create")
      _ <- whitespace
      _ <- stringInsensitive("table")
      - <- whitespace
      name <- identifier
      - <- char('(')
      columns <- columnListParser
      - <- char(')')
    } yield Table(name, columns)

  val columnparser: Parser[Column] =
    for {
      _ <- whitespace
      name <- identifier
      _ <- whitespace
      dtype <- identifier
      _ <-whitespace
      nullable <- stringInsensitive("NULL").or(stringInsensitive("NOT NULL")) .optional
      _ <- whitespace
      primary_key <- stringInsensitive("PRIMARY KEY").optional

      _ <- whitespace
    } yield Column(name, DataType.fromString(dtype).get, nullable.map {
      case "NULL" => true
      case _ => false
    }, primary_key.map{
      case "PRIMARY KEY" => true
      case _ => false
    } )


  val columnListParser: Parser[List[Column]] =
    for {
      first <- columnparser
      rest <- (comma.flatMap(_ => columnparser)).many
    } yield first :: rest


}
