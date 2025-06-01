package org.pwharned.server
import org.pwharned.http.HttpMethod.HttpMethod
import org.pwharned.http.generated.Headers
import org.pwharned.http.{HttpParser, HttpResponse}
import org.pwharned.route.*
import org.pwharned.route.Router.RouteDef

import java.io.PrintWriter
import java.net.{InetSocketAddress, Socket}
import java.nio.ByteBuffer
import java.nio.channels.{ServerSocketChannel, SocketChannel}
import java.util.concurrent.{ExecutorService, Executors}
import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}


def sendResponseAsync(socket: Socket, response: Future[HttpResponse])(using ec: ExecutionContext): Unit =
  val out = PrintWriter(socket.getOutputStream, true)
  response.onComplete{
    case Failure(exception) =>   out.println(s"HTTP/1.1 500 Bad Request")
    case Success(response) => {
      out.println(s"HTTP/1.1 ${response.status} OK")
      response.headers.asMap.foreach { case (key, value) => out.println(s"$key: $value") }
      out.println()
      out.println(response.body)
      socket.close()
    }
  }

def sendResponse(socket: Socket, response: HttpResponse): Unit =
    val out = PrintWriter(socket.getOutputStream, true)
        out.println(s"HTTP/1.1 ${response.status} OK")
        response.headers.asMap foreach { case (key, value) => out.println(s"$key: $value") }
        out.println()
        out.println(response.body)
        out.flush()
        socket.close()

def sendResponse(socket: SocketChannel, response: HttpResponse): Unit =
  val statusLine = s"HTTP/1.1 ${response.status} OK\r\n"
  val headers = response.headers.asMap.map { case (key, value) => s"$key: $value\r\n" }.mkString
  val body = s"\r\n${response.body}"
  val httpResponse = statusLine + headers + body
  // Convert string to bytes and wrap in ByteBuffer
  val buffer = ByteBuffer.wrap(httpResponse.getBytes("UTF-8"))


  // Write to socket channel
  while (buffer.hasRemaining) {
    socket.write(buffer)
  }

  // Close connection
  socket.close()

// Check if the buffer (up to its current position) contains the end-of-headers marker: CR LF CR LF (i.e. 13,10,13,10)
def hasEndOfHeaders(buffer: ByteBuffer): Boolean =
  val pos = buffer.position()
  if pos < 4 then false
  else
    val arr = buffer.array() // the underlying byte array
    var i = 0
    // Loop until position-4 (inclusive) to check for the 4-byte sequence
    while i <= pos - 4 do
      if arr(i) == 13 && arr(i + 1) == 10 && arr(i + 2) == 13 && arr(i + 3) == 10 then
        return true
      i += 1
    false

// Tail-recursive function to read from a channel until the end-of-headers marker is detected
@tailrec
def readUntilEndMarker(buffer: ByteBuffer, channel: SocketChannel): Unit =
  // Read more data into the buffer
  val bytesRead = channel.read(buffer)
  // If some bytes were read (or even 0 bytes), check for the marker
  if bytesRead > 0 || bytesRead == 0 then
    if !hasEndOfHeaders(buffer) then
      // Continue reading if the marker has not yet been found.
      readUntilEndMarker(buffer, channel)
// If bytesRead is negative, then the channel has reached EOF. In that case simply return.


object HTTPServer:
  private val executor = Executors.newVirtualThreadPerTaskExecutor()
  given ExecutionContext = ExecutionContext.fromExecutor(Executors.newVirtualThreadPerTaskExecutor())
  val ex: ExecutorService = Executors.newCachedThreadPool()
  inline def start(port: Int, inline routingTable: RoutingTable.RoutingTableType): Unit =

    val serverChannel = ServerSocketChannel.open()
    serverChannel.bind(new InetSocketAddress(port))


    while true do
      val clientChannel = serverChannel.accept()

      executor.execute(() =>

        // Allocate a ByteBuffer (adjust size as needed)
        val estimatedSize = clientChannel.socket().getReceiveBufferSize
        val buffer = ByteBuffer.allocate(Math.min(estimatedSize, 65536)) // Limit max size
        readUntilEndMarker(buffer,channel = clientChannel)
        buffer.flip()




        val request = HttpParser.parseRequest(buffer)
        // Later, you can handle POST (and other methods) appropriately by pattern matching on 'T'
        // Create key based on the incoming request.
        request match {
          case Some(req) => {

            val key = RoutingTable.lookup(routingTable, req.method, req.path)
            val response: Future[HttpResponse]  = key.map{
              x => x._1.asInstanceOf[RouteDef[HttpMethod]].handler.apply(req)
            }.getOrElse(Future(HttpResponse.notFound()))
            response.onComplete {
              case Failure(exception) => {
                println("Found Exception")
                sendResponse(clientChannel, HttpResponse.error(exception.toString))
                clientChannel.close()
              }
              case Success(value) => {
                sendResponse(clientChannel, value)
                clientChannel.close()
              }

            }
          }
          case None => {
            sendResponse(clientChannel, HttpResponse.error("Error reading client request"))
            clientChannel.close()
          }
        }


      )








