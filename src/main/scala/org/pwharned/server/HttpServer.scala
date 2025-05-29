import generated.user
import org.pwharned.Database
import org.pwharned.macros.{Db2TypeMapper, DbTypeMapper, createTable,streamQuery, seraialize}
import org.pwharned.server.{HTTPRequest, HTTPResponse, Handler}

import java.io.{BufferedReader, InputStreamReader, PrintWriter}
import java.net.{ServerSocket, Socket}
import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}


def sendResponse(socket: Socket, response: Future[HTTPResponse])(using ec: ExecutionContext): Unit =
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




def parseRequest(socket: Socket): HTTPRequest =
  val in = BufferedReader(InputStreamReader(socket.getInputStream))
  val requestLine = in.readLine().split(" ")
  val method = requestLine(0)
  val path = requestLine(1)

  var headers = Map.empty[String, String]
  var line: String = in.readLine()

  while (line != null && line.nonEmpty) do
    val parts = line.split(": ")
    headers = headers.updated(parts(0), parts(1))
    line = in.readLine()

  val body = if headers.contains("Content-Length") then
    val length = headers("Content-Length").toInt
    val bodyArray = Array.fill(length)(0.toByte)
    socket.getInputStream.read(bodyArray, 0, length)
    String(bodyArray)
  else ""

  HTTPRequest(method, path, headers, body)


object SimpleHTTPServer:
  private val executor = Executors.newVirtualThreadPerTaskExecutor()
  given ExecutionContext = ExecutionContext.fromExecutor(Executors.newVirtualThreadPerTaskExecutor())

  def start(port: Int, handler: Handler): Unit =
    val serverSocket = ServerSocket(port)

    while true do
      val clientSocket = serverSocket.accept()
      executor.execute(() =>
        val request = parseRequest(clientSocket)
        val response = handler(request)
        sendResponse(clientSocket, response)
      )




@main def runServer() =

  given ExecutionContext = ExecutionContext.fromExecutor(Executors.newVirtualThreadPerTaskExecutor())
  given DbTypeMapper = Db2TypeMapper
  val handler: Handler = req =>
    if req.path == "/" then  {
      val con = Database.getDbConnection()

      Future(createTable[user]).map(x => HTTPResponse.ok("Succesfully created")).recover {
        case ex: Exception => HTTPResponse.error(ex.toString)
      }

      }
    else if req.path =="/api/users" then {
      val con = Database.getDbConnection()
      val stream = con.streamQuery[user](5000).apply(con).map {
        x => x.flatMap(y => y.map(j =>j.seraialize) ).mkString(",")
      }

      stream.map(x =>  HTTPResponse.ok(s"""[$x]""" )).recover {
        case ex: Exception => HTTPResponse.error(ex.toString)
      }
    }
    else Future(HTTPResponse.notFound())

  SimpleHTTPServer.start(8080, handler)
