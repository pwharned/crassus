package org.pwharned.server
import org.pwharned.http.Headers.Headers
import org.pwharned.http.HttpMethod.HttpMethod
import org.pwharned.http.{Http, HttpPath, HttpRequest, HttpResponse, Protocal, Segment, SocketWriter, asRequest, toPath}
import org.pwharned.route.*
import org.pwharned.route.Router
import org.pwharned.route.given

import java.io.{BufferedOutputStream, PrintWriter}
import java.net.{InetSocketAddress, Socket}
import java.nio.ByteBuffer
import java.nio.channels.{ServerSocketChannel, SocketChannel}
import java.nio.charset.StandardCharsets
import java.util.concurrent.{ExecutorService, Executors, ThreadLocalRandom}
import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}
import org.pwharned.sql.HKD._

def sendResponse(socket: Socket, response: HttpResponse[?]): Unit =
  val bufferedOut = new BufferedOutputStream(socket.getOutputStream, 8192)
  val out = new PrintWriter(bufferedOut, false)

  out.print(s"HTTP/1.1 ${response.status} OK\r\n")
  response.headers.asMap.foreach { case (key, value) =>
    out.print(s"$key: $value\r\n")
  }
  out.print("\r\n")
  out.print(response.body)
  out.flush()
  bufferedOut.flush()
  socket.close()

def sendResponse(socket: SocketChannel, response: HttpResponse[?]): Unit =
  val responseBody = response.body.toString
  val contentLength = responseBody.getBytes(StandardCharsets.UTF_8).length

  val statusLine = s"HTTP/1.1 ${response.status} OK\r\n"
  val headers = response.headers.asMap.map { case (key, value) => s"$key: $value\r\n" }.mkString
  val fullResponse = statusLine + headers + s"Content-Length: $contentLength\r\n\r\n" + responseBody

  val responseBytes = fullResponse.getBytes(StandardCharsets.UTF_8)
  val buffer = ByteBuffer.wrap(responseBytes)

  while (buffer.hasRemaining) {
    socket.write(buffer)
  }
  socket.close()

// Find the position where headers end (after \r\n\r\n)
def findHeadersEndPosition(buffer: ByteBuffer): Int =
  val pos = buffer.position()
  if pos < 4 then -1
  else
    val arr = buffer.array()
    val arrayOffset = buffer.arrayOffset()
    var i = 0
    while i <= pos - 4 do
      val idx = arrayOffset + i
      if arr(idx) == 13 && arr(idx + 1) == 10 && arr(idx + 2) == 13 && arr(idx + 3) == 10 then
        return i + 4 // Return position after \r\n\r\n
      i += 1
    -1

def hasEndOfHeaders(buffer: ByteBuffer): Boolean =
  findHeadersEndPosition(buffer) != -1

@tailrec
def readUntilEndMarker(buffer: ByteBuffer, channel: Socket): Unit =
  val inputStream = channel.getInputStream
  val tempArray = new Array[Byte](4096)
  val bytesRead = inputStream.read(tempArray)

  if bytesRead > 0 then
    buffer.put(tempArray, 0, bytesRead)
    if !hasEndOfHeaders(buffer) then
      readUntilEndMarker(buffer, channel)

object HTTPServer:

  def extractContentLength(buffer: ByteBuffer): Int =
    val dup = buffer.duplicate()
    dup.flip()

    val headerBytes = new Array[Byte](dup.remaining())
    dup.get(headerBytes)
    val headersString = new String(headerBytes, StandardCharsets.UTF_8)

    val lines = headersString.split("\r\n")
    lines.find(line => line.toLowerCase.startsWith("content-length:")) match {
      case Some(line) =>
        val colonIndex = line.indexOf(':')
        if colonIndex >= 0 && colonIndex < line.length - 1 then
          val value = line.substring(colonIndex + 1).trim
          try {
            value.toInt
          } catch {
            case _: NumberFormatException => 0
          }
        else 0
      case None => 0
    }

  // NEW: Read remaining body bytes, accounting for what's already in buffer
  def readRemainingBody(buffer: ByteBuffer, channel: Socket, totalContentLength: Int): Unit =
    // Find where headers end
    val headersEndPos = findHeadersEndPosition(buffer)
    if headersEndPos == -1 then
      throw new RuntimeException("Headers end marker not found")

    // Calculate how much body data we already have
    val currentPos = buffer.position()
    val bodyBytesAlreadyRead = currentPos - headersEndPos
    val remainingBodyBytes = totalContentLength - bodyBytesAlreadyRead

    println(s"Content-Length: $totalContentLength, already read: $bodyBytesAlreadyRead, remaining: $remainingBodyBytes")

    // Only read more if we need more bytes
    if remainingBodyBytes > 0 then
      var totalRead = 0
      val inputStream = channel.getInputStream

      while totalRead < remainingBodyBytes do
        val stillNeed = remainingBodyBytes - totalRead
        val bufferSize = Math.min(stillNeed, 4096)
        val tempArray = new Array[Byte](bufferSize)

        val bytesRead = inputStream.read(tempArray, 0, Math.min(tempArray.length, stillNeed))

        if bytesRead > 0 then
          buffer.put(tempArray, 0, bytesRead)
          totalRead += bytesRead
        else if bytesRead == -1 then
          throw new RuntimeException(s"Unexpected EOF: read $totalRead of $remainingBodyBytes remaining bytes (already had $bodyBytesAlreadyRead)")
        else
          // bytesRead == 0, try again (shouldn't happen with blocking I/O but just in case)
          Thread.sleep(1)

  val ex: ExecutorService = {
    val cores = Runtime.getRuntime.availableProcessors()
    val poolSize = Math.max(cores * 2, 8)
    Executors.newFixedThreadPool(poolSize, r => {
      val t = new Thread(r)
      t.setDaemon(true)
      t.setName(s"http-server-${ThreadLocalRandom.current().nextInt()}")
      t
    })
  }

  given ExecutionContext = ExecutionContext.fromExecutor(ex)

  inline def start(inline port: Int, inline routingTable: RoutingTable.RoutingTable[HttpMethod,Protocal]): Unit =
    val serverChannel = ServerSocketChannel.open()

    val serverSocket = serverChannel.socket()
    serverSocket.setReuseAddress(true)
    serverSocket.setReceiveBufferSize(65536)

    serverChannel.bind(new InetSocketAddress(port), 128)

    println(s"HTTP Server started on port $port with ${ex.asInstanceOf[java.util.concurrent.ThreadPoolExecutor].getCorePoolSize} threads")

    while true do
      val clientChannel = serverChannel.accept()

      ex.execute(() =>
        var buffer: ByteBuffer = null
        try {
          val clientSocket = clientChannel.socket()

          clientSocket.setTcpNoDelay(true)
          clientSocket.setSoTimeout(30000)

          buffer = ByteBuffer.allocate(65536)

          // 1. Read until we have complete headers
          readUntilEndMarker(buffer, clientSocket)

          // 2. Extract Content-Length
          val contentLength = extractContentLength(buffer)

          // 3. Read remaining body bytes (accounting for what we already read)
          if contentLength > 0 then
            readRemainingBody(buffer, clientSocket, contentLength)

          // 4. Parse the complete request
          buffer.flip()
          val request = buffer.asRequest

          request match {
            case Some(req) =>
              val method = req.method
              val path = req.path
              val key = routingTable.find(method, path)

              val response = key.flatMap {
                _.route.map { route =>
                  route.processRequest(clientChannel, req)
                }
              }.getOrElse({
                Future(HttpResponse.notFound)
              }.flatMap(res => summon[SocketWriter[Http]].write(clientChannel, res)))

              response.onComplete {
                case Failure(exception) =>
                  println(s"Request failed: ${exception.getMessage}")
                  exception.printStackTrace()
                  try {
                    summon[SocketWriter[Http]].write(clientChannel, HttpResponse.error(exception.getMessage))
                  } finally {
                    try clientChannel.close() catch case _: Exception => ()
                  }
                case Success(_) =>
                // Success handled by route processing
              }

            case None =>
              println("Failed to parse request")
              sendResponse(clientChannel, HttpResponse.error("Error parsing client request"))
              try clientChannel.close() catch case _: Exception => ()
          }

        } catch {
          case ex: Exception =>
            println(s"Connection error: ${ex.getMessage}")
            ex.printStackTrace()
            try clientChannel.close() catch case _: Exception => ()
        }
      )

  sys.addShutdownHook {
    println("Shutting down HTTP server...")
    ex.shutdown()
  }
