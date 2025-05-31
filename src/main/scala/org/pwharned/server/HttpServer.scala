package org.pwharned.server
import generated.user
import org.pwharned.database.Database
import org.pwharned.http.{HttpRequest, HttpResponse}
import org.pwharned.macros.{Db2TypeMapper, DbTypeMapper, response}
import org.pwharned.route.Router.HttpMethod.GET
import org.pwharned.route.Router.{HttpMethod, Route, route}
import org.pwharned.route.*

import java.io.{BufferedReader, InputStreamReader, PrintWriter}
import java.net.{ServerSocket, Socket}
import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}


def sendResponseAsync(socket: Socket, response: Future[HttpResponse])(using ec: ExecutionContext): Unit =
  val out = PrintWriter(socket.getOutputStream, true)
  response.onComplete{
    case Failure(exception) =>   out.println(s"HTTP/1.1 500 Bad Request")
    case Success(response) => {
      out.println(s"HTTP/1.1 ${response.status} OK")
      response.headers.foreach { case (key, value) => out.println(s"$key: $value") }
      out.println()
      out.println(response.body)
      socket.close()
    }
  }

def sendResponse(socket: Socket, response: HttpResponse): Unit =
    val out = PrintWriter(socket.getOutputStream, true)
        out.println(s"HTTP/1.1 ${response.status} OK")
        response.headers.foreach { case (key, value) => out.println(s"$key: $value") }
        out.println()
        out.println(response.body)
        out.flush()
        socket.close()

import java.io.ByteArrayOutputStream

def parseRequest(socket: Socket): Option[HttpRequest] = {
  val in = BufferedReader(InputStreamReader(socket.getInputStream))
  val requestLine = in.readLine()

  if (requestLine == null) {
    return None

  }
  val requestLineString = requestLine.split(" ")
  val method = requestLineString(0)
  val path = requestLineString(1)

  var headers = Map.empty[String, String]
  var line: String = in.readLine()

  while (line != null && line.nonEmpty) {
    val parts = line.split(": ")
    headers = headers.updated(parts(0), parts(1))
    line = in.readLine()
  }

  val body = if (headers.contains("Content-Length")) {
    val length = headers("Content-Length").toInt
    val output = ByteArrayOutputStream()
    socket.getInputStream.transferTo(output)
    output.toString
  } else ""

  Some(HttpRequest(method, path, headers, body))
}




object HTTPServer:
  private val executor = Executors.newVirtualThreadPerTaskExecutor()
  given ExecutionContext = ExecutionContext.fromExecutor(Executors.newVirtualThreadPerTaskExecutor())
  val ex: ExecutorService = Executors.newCachedThreadPool()
  inline def start[T<: HttpMethod](port: Int, inline routingTable: RoutingTable.RoutingTableType): Unit =
    val serverSocket = ServerSocket(port)

    while true do
      val clientSocket = serverSocket.accept()
      executor.execute(() =>
        val request = parseRequest(clientSocket)
        // Later, you can handle POST (and other methods) appropriately by pattern matching on 'T'
        // Create key based on the incoming request.
        request match {
          case Some(req) => {

            val key = RoutingTable.keyFor(req.method, req.path)
            val response: Future[HttpResponse] =
              routingTable
                .get(key)
                .map(route => route(req))
                .getOrElse(Future(HttpResponse(404, Map.empty, "Not Found")))
            response.onComplete {
              case Failure(exception) => {
                println("Found Exception")
                sendResponse(clientSocket, HttpResponse.error(exception.toString))
                clientSocket.close()
              }
              case Success(value) => {
                sendResponse(clientSocket, value)
                clientSocket.close()
              }

            }
          }
          case None => {
            sendResponse(clientSocket, HttpResponse.error("Error reading client request"))
            clientSocket.close()
          }
        }


      )




@main def runServer() =
  // Import the DSL extension.



  // Compose routes using the '~' operator; note that the result is a tuple.
  given ExecutionContext = ExecutionContext.fromExecutor(Executors.newVirtualThreadPerTaskExecutor())
  given DbTypeMapper = Db2TypeMapper

  val handler: Route[HttpMethod] = route(GET, "/", (req: HttpRequest) => Future(HttpResponse.ok("Hello")))

  val userHandler:  Route[HttpMethod] = route(GET, "/users", (req: HttpRequest) => {
    Database.response[user]
  }   )
  val table = RoutingTable.build(List(handler, userHandler))

  HTTPServer.start(8080, table)




    // Use the response...



