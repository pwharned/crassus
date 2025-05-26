import org.pwharned.{Column, DataType, Table, columnListParser, columnparser, createTableParser}
object Test extends App:
  val tableParsed = createTableParser("create table mytable(id integer, test text)")
  tableParsed match
    case Left(value) => throw new Exception(value.toString)
    case Right(value) => println(value)

  val colParsed = columnparser("ID INTEGER")
  colParsed match
    case Left(value) => throw new Exception(value.toString)
    case Right(value) => value._1.name == "ID"


  val colListParsed = columnListParser("ID INTEGER, ID INTEGER")
  colListParsed match
    case Left(value) => throw new Exception(value.toString)
    case Right(value) => assert(value._1.head == Column("ID", DataType.fromString("INTEGER").get, Some(true)))
                        assert(value._1.length ==2)

