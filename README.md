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

# Automatic Route Generation, HTTP Server, JSON Serialization/Deserialization

Ultimate goal is to have a no boilerplate HTTP server that automatically generates routes corresponding to all basic CRUD operations on a given table. For example, given the following table:

```
create table user(id: int not null primary key, name: String)
```

We automagically generate the following endpoints:

```
GET /api/user -> returns a list of all users
POST /api/user -> create a user
PATCH /api/user/{user_id} updates a user
DELETE /api/user/{user_id} delete a user
GET /api/user/{user_id} get a particular user
```


Currently this is achievable with the following:


```
@main def runServer() =
  // Import the DSL extension.



  // Compose routes using the '~' operator; note that the result is a tuple.
  given ExecutionContext = ExecutionContext.fromExecutor(Executors.newVirtualThreadPerTaskExecutor())
  given DbTypeMapper = Db2TypeMapper


  val table: RoutingTable.RoutingTableType = RoutingTable.build(RouteRegistry.getRoutes[user])

  HTTPServer.start(8080, table)

```

We use a very simple HTTP server that is nonetheless highly concurrent, I am not an expert at evaluating these things but at the moment it is outperforming my rust server by a significant margin, finishing 10k GET Requests to a table with 100 users in around ten seconds, which corresponds to almost 1k requests per seconds runing on my laptop.

```

❯ python test.py --url "http://localhost:8080/api/user" --requests 10000
Testing http://localhost:8080/api/user with 1000 concurrent connections
Total requests: 10000
Started at: 20:30:57

Results:
Total time: 10.50 seconds
Successful requests: 10000 (100.0%)
Failed requests: 0 (0.0%)
Requests per second: 952.45
Average response time: 5747.30 ms
Min response time: 1596.13 ms
Max response time: 9410.24 ms
Finished at: 20:31:08

```
```
❯ python test.py --url "http://localhost:8080/api/user" --requests 20000 --concurrency 1000
Testing http://localhost:8080/api/user with 1000 concurrent connections
Total requests: 20000
Started at: 20:43:14

Results:
Total time: 24.16 seconds
Successful requests: 20000 (100.0%)
Failed requests: 0 (0.0%)
Requests per second: 827.89
Average response time: 13001.52 ms
Min response time: 2914.64 ms
Max response time: 21827.19 ms
Finished at: 20:43:38

```

