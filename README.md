# Crassus

Demonstrating how to use Scala3 macros and mirrors to 
1. Automatically generate case classes that reflect the structure of a table in a sql databse
2. Automatically generate implementations for retrieving intances of the case class from java.sql.Result set in a typesafe manner

# Case class generation

Since Dotty does not support generation of case classes which are visible outside of the scope of the macro expansion, we use a multi stage build project. The caseClassGenerator uses a simple parser
combinator to take a SQL DDL statement and transform it into an in internal parsable structure. From here, we simply write the case class definition to a source file visible in the main project,
mapping the SQL datatypes to their Scala/Java representation.

# Inline retrieval methods using Mirror type class derivation

Using Scalas Mirror we can inspect the fields and types of any Product(ie case class) at compile time and derive the correct select statement for the given case class, as well as a typesafe way 
to map a ResultSet to an instance of a class. We can take it a step further using extension methods, allowing for the following:

```
extension (rs: java.sql.ResultSet)
  inline def as[A <: Product](using sql: Sql[A]): A =
    sql.fromResultSet(rs)


extension (con: java.sql.Connection)
  inline def streamQuery[A <: Product](batchSize: Int)(using sql: Sql[A]): java.sql.Connection => Iterator[Seq[A]] = con =>
    val stmt = con.prepareStatement(sql.select)
    val rs = stmt.executeQuery()

    Iterator.continually(rs.next())
      .takeWhile(identity)
      .map( x => rs.as[A]).grouped(batchSize)
```

```
    val userStream = conn.streamQuery[mytablev](batchSize = 5000)
```
Returning a lazily evaluated stream of instances of our case class.


For any other kind of type we would need to retrive from the database, the only thing that is required is to add the ddl for the table. The entire implementation is ~200 lines of code. I hope to expand it
to a full fledged web server using Netty
