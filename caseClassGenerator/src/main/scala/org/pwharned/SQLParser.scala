package org.pwharned

object SQLParser extends Parse {

  implicit class ColumnOps(column: Column) {
    def toField: String = {
      val typeStr = column.nullable.getOrElse(true) match {
        case true => s"Nullable[${column.dataType.scalaType}]"
        case false =>
          column.default.getOrElse(false) match {
            case true  => s"Default[${column.dataType.scalaType}]"
            case false => column.dataType.scalaType
          }
      }

      val defaultOrGenerated = column.default.getOrElse(
        false
      ) | column.generated_always_as_identity.getOrElse(false)

      // Add annotation for primary key fields
      column.primary_key match {
        case Some(true) => {
          defaultOrGenerated match {
            case true =>
              s" `${column.name}`: F[GeneratedPrimaryKey[${column.dataType.scalaType}]]"
            case false =>
              s" `${column.name}`: F[PrimaryKey[${column.dataType.scalaType}]]"
          }

        }
        case _ => s"`${column.name}`: F[$typeStr]"
      }
    }
    def toFieldLower: String = {
      val typeStr = column.nullable.getOrElse(true) match {
        case true  => s"Option[${column.dataType.scalaType}]"
        case false => s"${column.dataType.scalaType}"
      }

      // Add annotation for primary key fields
      column.primary_key match {
        case Some(true) => s" `${column.name}`: ${column.dataType.scalaType}"
        case _          => s"`${column.name}`: $typeStr"
      }
    }
  }

  def typeParser: Parser[SqlDataType] = {
    val allParsers = SqlDataType.values.map(x => x.parse)
    def tryParsers(
        remaining: List[Parser[SqlDataType]],
        p: Parser[SqlDataType]
    ): Parser[SqlDataType] = {
      if (remaining.isEmpty) {
        p
      } else {
        tryParsers(remaining.tail, p.or(remaining.head))
      }
    }

    tryParsers(allParsers.tail, allParsers.head)
  }
  
  val commentOnTableParser: Parser[Comment] = {
    for {
      schema <- identifier
      _ <- whitespace
      _ <- char('.')
      _ <- whitespace
      tableName <- identifier
      _ <- whitespace
      _ <- stringInsensitive("IS")
      _ <- whitespace
      comment <- singleQuotedString
    } yield Comment(schema, tableName, None, comment)
  }

 val commentOnColumnParser : Parser[Comment]= {
    for {
      schema <- identifier
      _ <- whitespace
      _ <- char('.')
      _ <- whitespace
      tableName <- identifier
      _ <- whitespace
      _ <- char('.')
      _ <- whitespace
      columnName <- identifier
      _ <- whitespace
      _ <- stringInsensitive("IS")        // you forgot this in your code
      _ <- whitespace
      comment <- singleQuotedString

    }yield Comment(schema, tableName, Some(columnName), comment) 

  }


  val commentParser: Parser[Comment] = 
    for {
      _ <- whitespace
      _ <- stringInsensitive("COMMENT ON")
      _ <- whitespace
      kind <- stringInsensitive("TABLE").or(stringInsensitive("COLUMN"))
      _ <- whitespace
      comment <- kind.toLowerCase match {
      case "table"  => commentOnTableParser
      case "column" => commentOnColumnParser
    }
    }
  yield comment
  val functionParser: Parser[String] =
    for {
      f <- identifier
      _ <- char('(')
      _ <- char(')')
    } yield f + "()"

  val doubleParser: Parser[String] =
    for {
      m <- numeric
      _ <- char('.')
      n <- numeric
    } yield s"$m.$n"
  val booleanParser: Parser[String] =
    stringInsensitive("true").or(stringInsensitive("false"))
  
  val defaultParser: Parser[String] =
    for {
      _ <- whitespace
      _ <- stringInsensitive("DEFAULT").optional
      _ <- whitespace
      default <- functionParser.or(doubleParser).or(booleanParser)
    } yield default

  val createTableParser: Parser[Table] =
    for {
      _ <- whitespace
      _ <- stringInsensitive("create")
      _ <- whitespace
      _ <- stringInsensitive("table")
      - <- whitespace
      schema <- identifier.optional
      _ <- whitespace
      _ <- char('.').optional
      name <- identifier
      _ <- whitespace
      - <- char('(')
      _ <- whitespace
      columns <- columnListParser
      _ <- whitespace.optional
      - <- char(')')
      _ <- whitespace.optional
    } yield Table(name, columns, schema)

  val nullableParser: Parser[String] =
    for {
      _ <- whitespace
      not <- stringInsensitive("NOT").optional
      _ <- whitespace
      n <- stringInsensitive("NULL").optional
    } yield (not, n) match {
      case (Some(_), Some(_)) => "NOT NULL"
      case (None, Some(_))    => "NULL"
      case _                  => ""
    }

  val columnparser: Parser[Column] =
    for {
      _ <- whitespace
      name <- identifier
      _ <- whitespace
      dtype <- typeParser
      _ <- whitespace
      default <- defaultParser.optional
      _ <- whitespace
      nullable <- nullableParser.optional
      _ <- whitespace
      primary_key <- stringInsensitive("PRIMARY KEY").optional
      _ <- whitespace
      identity <- stringInsensitive("GENERATED ALWAYS AS IDENTITY").optional
      _ <- whitespace.optional
    } yield Column(
      name,
      dtype,
      nullable.map {
        case "NOT NULL" => false
        case "NULL"     => true
        case _          => true
      },
      primary_key.map {
        case "PRIMARY KEY" => true
        case _             => false
      },
      identity.map {
        case "GENERATED ALWAYS AS IDENTITY" => true
        case _                              => false
      },
      Some(default.isDefined)
    )

  val alterTableAddGeneratedAlwaysAsIdentity
      : Parser[GeneratedAlwaysAsIdentity] =
    for {
      _ <- whitespace
      _ <- stringInsensitive("ALTER")
      _ <- whitespace
      _ <- stringInsensitive("TABLE")
      - <- whitespace
      - <- stringInsensitive("ONLY").optional
      _ <- whitespace
      schema <- identifier
      _ <- whitespace
      _ <- char('.')
      _ <- whitespace
      table <- identifier
      _ <- whitespace
      _ <- stringInsensitive("ALTER")
      - <- whitespace
      _ <- stringInsensitive("COLUMN")
      _ <- whitespace
      colName <- identifier
      _ <- whitespace
      _ <- stringInsensitive("ADD")
      - <- whitespace
      _ <- stringInsensitive("GENERATED")
      _ <- whitespace
      _ <- stringInsensitive("ALWAYS")
      _ <- whitespace
      _ <- stringInsensitive("AS")
      _ <- whitespace
      _ <- stringInsensitive("IDENTITY")
      _ <- whitespace
      _ <- char('(')
      _ <- takeUntilString(")")

    } yield GeneratedAlwaysAsIdentity(schema, table, colName)

  val columnListParser: Parser[List[Column]] =
    for {
      first <- columnparser
      rest <- (comma.flatMap(_ => columnparser)).many
    } yield first :: rest

  val columnNameListParser: Parser[List[String]] =
    for {
      first <- identifier

      rest <- (comma.flatMap(_ => whitespace.flatMap(_ => identifier))).many
    } yield first :: rest

  val alterTablePrimaryKeyParser: Parser[PrimaryKey] =
    for {
      _ <- whitespace
      _ <- stringInsensitive("ALTER")
      _ <- whitespace
      _ <- stringInsensitive("TABLE")
      - <- whitespace
      - <- stringInsensitive("ONLY").optional
      _ <- whitespace
      schema <- identifier
      _ <- whitespace
      _ <- char('.')
      _ <- whitespace
      table <- identifier
      _ <- whitespace
      _ <- stringInsensitive("ADD")
      - <- whitespace
      _ <- stringInsensitive("CONSTRAINT")
      _ <- whitespace
      primaryKeyName <- identifier
      _ <- whitespace
      _ <- stringInsensitive("PRIMARY")
      - <- whitespace
      _ <- stringInsensitive("KEY")
      _ <- whitespace
      _ <- char('(')
      columns <- columnNameListParser
      _ <- whitespace
      _ <- char(')')
    } yield PrimaryKey(schema, table, columns)

}
