# Crassus

A fully contained, self documenting framework for developing  REST and RPC APIs in scala3. 

This project has zero dependencies, everything is written from scratch in scala3.

It includes:

1. A simple web server built using JVM virtual threads
2. Json serialization/Deserialization
3. A parser and workflow for automatically generating HTTP routes based on SQL DDL ( Db2 and Postgres)
4. Automatic OpenAPI generation


# Create a simple route

Following is an example of how to create a simple route based on a SQL statement

```
// Create an execution context and a type mapper to map Sql Types to Postgres - Db2 is also supported
given ExecutionContext = ExecutionContext.fromExecutor(Executors.newVirtualThreadPerTaskExecutor())
given DbTypeMapper = PostgresTypeMapper
given Database.type = Database

// load some database connection details.
val connectionDetails = EnvLoader.loadFromEnvFile[ConnectionDetails](".env") match {
case Right(details) => details
case Left(error) =>
  println(s"Error: $error")
  sys.exit(1)
}
// create a pool
Database.createPool(connectionDetails)

// create a small case class to capture thee results from the database.

case class parent_child(child: String, Parent: String)

// specify the select statement that goes with this
object parent_child:
  given SelectStatement[parent_child]:
    override def select: String = "select a.name as child, b.name as parent from attributes a join parent c on c.caid = a.id join parent p on p.paid = c.paid join attributes b on b.id = p.paid"

// create the route
inline def parent_child_route = RouteRegistry.get[Http, IdHKD[parent_child]]
val routes = List(parent_child_route)

// build and serve the routing table
lazy val table  = RoutingTable.build(routes.map( x=> Lazy(() => x)))
HTTPServer.start(8080, table)

```

# Creating Routes with Automatic Openapi generaiton



```

  inline def swagger = route[Http, GET, Unit, String](GET, "/doc/openapi".asPath, (req: HttpRequest[Unit]) => Future {
    val source = scala.io.Source.fromFile("static/index.html")
    HttpResponse (body = Body.text(source.getLines().mkString), headers = Headers(Map("content-type"-> "text/html")))

  })
  
  
  val routes = List(assetAttributeRoute, parent_child_route, r, swagger, openapi)


  import java.io.PrintWriter

  val pw = new PrintWriter("static/openapi.json") // opens (or creates) the file
  try {
    pw.write(routes.toOpenApi.serialize)
  } finally {
    pw.close() // always close to flush and free resources
  }
  
  lazy val table  = RoutingTable.build(routes.map( x=> Lazy(() => x)))
  HTTPServer.start(8080, table)

  
```
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

We can create a no boilerplate HTTP server that automatically generates routes corresponding to all basic CRUD operations on a given table. For example, given the following table:

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

A simple but highly concurrent and fault tolerant HTTP server:
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

