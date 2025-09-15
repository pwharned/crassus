package org.pwharned.http.server

import org.pwharned.codec.Codec
import org.pwharned.http.HttpTypes.{HeaderName, HttpPath}
import org.pwharned.http.*
import org.pwharned.io.IO

import java.nio.ByteBuffer
import java.nio.channels.{SelectionKey, SocketChannel}
import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedQueue, Executors}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import scala.collection.mutable
import org.pwharned.stream.Stream

import scala.concurrent.ExecutionContext

class HttpServer(port: Int):
  private val server = Server(port)
  private val connectionStates = mutable.Map[SelectionKey, ConnectionState]()
  private val registry = new RouteRegistry()
  @volatile private var running = true

  // Virtual thread executor for request processing
  private val virtualThreadExecutor = Executors.newVirtualThreadPerTaskExecutor()
  private given ExecutionContext = ExecutionContext.fromExecutor(virtualThreadExecutor)

  // Lock-free buffer pools
  private val readBufferPool = ConcurrentLinkedQueue[ByteBuffer]()
  private val accumulatedBufferPool = ConcurrentLinkedQueue[ByteBuffer]()
  private val readPoolSize = AtomicInteger(0)
  private val accPoolSize = AtomicInteger(0)
  private val maxPoolSize = 200

  // Pre-allocated response pools
  private val notFound404Pool = ConcurrentLinkedQueue[ByteBuffer]()
  private val error500Pool = ConcurrentLinkedQueue[ByteBuffer]()
  private val badRequest400Pool = ConcurrentLinkedQueue[ByteBuffer]()

  // Route cache for fast lookup
  private val routeCache = ConcurrentHashMap[String, Route[?, ?]]()

  // Selector optimizations
  private val pendingWrites = ConcurrentLinkedQueue[SelectionKey]()
  private val needsWakeup = AtomicBoolean(false)

  // Cached HTTP delimiter for efficiency
  private val httpDelimiter = "\r\n\r\n".getBytes()

  // Initialize response pools
  locally {
    for (_ <- 1 to 20) {
      val notFound = HttpResponse.notFound("Route not found")
      notFound404Pool.offer(ByteBuffer.wrap(notFound.toBytes(using Codec.stringCodec)))

      val serverError = HttpResponse.internalError("Internal server error")
      error500Pool.offer(ByteBuffer.wrap(serverError.toBytes(using Codec.stringCodec)))

      val badRequest = HttpResponse.badRequest("Bad request")
      badRequest400Pool.offer(ByteBuffer.wrap(badRequest.toBytes(using Codec.stringCodec)))
    }
  }

  // Lock-free buffer acquisition
  private def acquireReadBuffer(): ByteBuffer = {
    val buffer = readBufferPool.poll()
    if (buffer != null) {
      readPoolSize.decrementAndGet()
      buffer.clear()
      buffer
    } else {
      ByteBuffer.allocate(8192)
    }
  }

  private def acquireAccumulatedBuffer(): ByteBuffer = {
    val buffer = accumulatedBufferPool.poll()
    if (buffer != null) {
      accPoolSize.decrementAndGet()
      buffer.clear()
      buffer
    } else {
      ByteBuffer.allocate(16384)
    }
  }

  private def releaseReadBuffer(buffer: ByteBuffer): Unit = {
    if (readPoolSize.get() < maxPoolSize && buffer.capacity() == 8192) {
      readBufferPool.offer(buffer)
      readPoolSize.incrementAndGet()
    }
  }

  private def releaseAccumulatedBuffer(buffer: ByteBuffer): Unit = {
    if (accPoolSize.get() < maxPoolSize && buffer.capacity() == 16384) {
      accumulatedBufferPool.offer(buffer)
      accPoolSize.incrementAndGet()
    }
  }

  // Optimized route registration with caching
  def route[A, B](method: HttpMethod, path: HttpPath)(
    handler: HttpRequest[A] => IO[HttpResponse[B]]
  )(using requestCodec: Codec[A], responseCodec: Codec[B]): Unit = {
    val route = Route(method, path, handler)(using requestCodec, responseCodec)
    registry.register(route)

    val key = s"${method.value}:${path.value}"
    routeCache.put(key, route)
  }

  private def findRouteFast(method: HttpMethod, path: HttpPath): Option[Route[?, ?]] = {
    val key = s"${method.value}:${path.value}"
    Option(routeCache.get(key)).orElse(registry.findRoute(method, path))
  }

  // Convenience methods
  def get[B](path: String)(handler: HttpRequest[Unit] => IO[HttpResponse[B]])(using codec: Codec[B]): Unit =
    route(HttpMethod.GET, HttpPath(path))(handler)(using Codec.unitCodec, codec)

  def post[A, B](path: String)(handler: HttpRequest[A] => IO[HttpResponse[B]])(using requestCodec: Codec[A], responseCodec: Codec[B]): Unit =
    route(HttpMethod.POST, HttpPath(path))(handler)

  def put[A, B](path: String)(handler: HttpRequest[A] => IO[HttpResponse[B]])(using requestCodec: Codec[A], responseCodec: Codec[B]): Unit =
    route(HttpMethod.PUT, HttpPath(path))(handler)

  def delete[B](path: String)(handler: HttpRequest[Unit] => IO[HttpResponse[B]])(using codec: Codec[B]): Unit =
    route(HttpMethod.DELETE, HttpPath(path))(handler)(using Codec.unitCodec, codec)

  // SSE route registration - Fixed typing
  def getSSE[A](path: String)(handler: HttpRequest[Unit] => IO[Stream[IO[A]]])(using codec: Codec[A]): Unit = {
    // Create a codec for streaming responses
    given streamCodec: Codec[Stream[IO[String]]] = new Codec[Stream[IO[String]]] {
      def decode(slice: HttpTypes.ByteSlice): Either[String, Stream[IO[String]]] =
        Left("SSE streams are output only")
      def encode(value: Stream[IO[String]]): Array[Byte] =
        "".getBytes() // Headers only, actual streaming handled separately
      def contentType: String = "text/event-stream"
    }

    route(HttpMethod.GET, HttpPath(path)) { request =>
      handler(request.asInstanceOf).map { dataStream =>
        // Convert the data stream to SSE format
        val sseStream = dataStream.map(_.map { value =>
          val encoded = new String(codec.encode(value))
          s" $encoded\n\n"
        })

        // Create response with stream
        HttpResponse(
          StatusCode.Ok,
          Map(
            HeaderName("Content-Type") -> "text/event-stream",
            HeaderName("Cache-Control") -> "no-cache",
            HeaderName("Connection") -> "keep-alive"
          ),
          sseStream
        )
      }
    }(using Codec.unitCodec.asInstanceOf, streamCodec)
  }

  def start(): IO[Unit] =
    for {
      _ <- server.bind()
      _ <- IO.println(s"HTTP server started on port $port")
      _ <- optimizedEventLoop()
    } yield ()

  def shutdown(): IO[Unit] =
    IO.effect({
      running = false
    }) *>
      IO.effect(virtualThreadExecutor.shutdown()) *>
      server.close()

  // Optimized event loop with batched processing
  private def optimizedEventLoop(): IO[Unit] = IO.effect {
    while (running) {
      try {
        // Reset wakeup flag at start of cycle
        needsWakeup.set(false)

        // Process pending writes first
        processPendingWrites()

        // Get multiple events in one select call
        val events = server.waitForEventsMultiple()
        events.foreach(handleServerEventDirect)

      } catch {
        case _: InterruptedException | _: java.nio.channels.ClosedSelectorException =>
          running = false
        case ex: Exception =>
          println(s"Event loop error: ${ex.getMessage}")
      }
    }
  }

  // Process queued writes in batches
  private def processPendingWrites(): Unit = {
    var key = pendingWrites.poll()
    var processedCount = 0

    while (key != null && processedCount < 100) { // Batch limit
      handleWriteDirect(key)
      key = pendingWrites.poll()
      processedCount += 1
    }
  }

  private def handleServerEventDirect(event: ServerEvent): Unit = event match {
    case ServerEvent.NewConnection(connection, key) =>
      connectionStates(key) = ConnectionState.Reading(
        acquireReadBuffer(),
        acquireAccumulatedBuffer()
      )

    case ServerEvent.DataReady(key) =>
      handleReadDirect(key)

    case ServerEvent.WriteReady(key) =>
      handleWriteDirect(key)

    case ServerEvent.NoEvent =>
    // Do nothing
  }

  private def handleReadDirect(key: SelectionKey): Unit = {
    connectionStates.get(key) match {
      case Some(ConnectionState.Reading(buffer, accumulated)) =>
        val connection = Connection(key.channel().asInstanceOf[SocketChannel])
        val bytesRead = connection.read(buffer).unsafeRun()

        if (bytesRead == -1) {
          cleanupConnectionUnsafe(key)
        } else if (bytesRead > 0) {
          buffer.flip()
          accumulated.put(buffer)
          buffer.clear()
          checkForCompleteRequestDirect(key, accumulated)
        }
      case Some(ConnectionState.Processing(_)) =>
      // Ignore additional reads during processing
      case _ =>
        cleanupConnectionUnsafe(key)
    }
  }

  private def checkForCompleteRequestDirect(key: SelectionKey, accumulated: ByteBuffer): Unit = {
    accumulated.flip()
    val hasCompleteRequest = findSequenceOptimized(accumulated, httpDelimiter) != -1
    accumulated.rewind()

    if (hasCompleteRequest) {
      processRequestAsyncDirect(key, accumulated)
    } else {
      accumulated.compact()
    }
  }

  // Optimized byte sequence search
  private def findSequenceOptimized(buffer: ByteBuffer, pattern: Array[Byte]): Int = {
    val limit = buffer.limit() - pattern.length + 1
    val patternLength = pattern.length
    var i = buffer.position()

    while (i < limit) {
      if (buffer.get(i + patternLength - 1) == pattern(patternLength - 1)) {
        var matches = true
        var j = 0
        while (j < patternLength - 1 && matches) {
          if (buffer.get(i + j) != pattern(j)) {
            matches = false
          }
          j += 1
        }
        if (matches) return i
      }
      i += 1
    }
    -1
  }

  private def processRequestAsyncDirect(key: SelectionKey, buffer: ByteBuffer): Unit = {
    connectionStates(key) = ConnectionState.Processing(buffer)

    virtualThreadExecutor.execute(() => {
      try {
        val responseBytes = processRequestInVirtualThread(buffer, key)
        handleAsyncResponse(key, responseBytes)
      } catch {
        case ex: Exception =>
          handleAsyncError(key, ex)
      }
    })
  }

  // Updated to include key parameter
  private def processRequestInVirtualThread(buffer: ByteBuffer, key: SelectionKey): ByteBuffer = {
    try {
      HttpRequest.parseRawRequest(buffer) match {
        case Left(error) =>
          getPooledErrorResponse(StatusCode.BadRequest)
        case Right((method, path)) =>
          processTypedRequestInVirtualThread(buffer, method, path, key)
      }
    } catch {
      case ex: Exception =>
        getPooledErrorResponse(StatusCode.InternalServerError)
    }
  }

  // Updated to include key parameter
  private def processTypedRequestInVirtualThread(buffer: ByteBuffer, method: HttpMethod, path: HttpPath, key: SelectionKey): ByteBuffer = {
    findRouteFast(method, path) match {
      case None =>
        getPooledErrorResponse(StatusCode.NotFound)
      case Some(route) =>
        handleTypedRouteInVirtualThread(buffer, route, key)
    }
  }

  // Fixed: Updated to handle streaming responses properly
  private def handleTypedRouteInVirtualThread(buffer: ByteBuffer, route: Route[?, ?], key: SelectionKey): ByteBuffer = {
    route match {
      case r: Route[a, b] =>
        given requestCodec: Codec[a] = r.requestCodec
        given responseCodec: Codec[b] = r.responseCodec

        HttpRequest.parse[a](buffer).unsafeRun() match {
          case Left(error) =>
            getPooledErrorResponse(StatusCode.BadRequest)
          case Right(request) =>
            try {
              val response = r.handler(request).unsafeRun()

              // Check if response body is a stream by checking the content type
              val isSSE = response.headers.get(HeaderName("Content-Type")).contains("text/event-stream")

              if (isSSE) {
                // Handle streaming response
                response.body match {
                  case stream: Stream[IO[String]] =>
                    startStreamingResponse(key, stream)
                    createStreamingHeaders(response)
                  case _ =>
                    // Fallback to normal response
                    ByteBuffer.wrap(response.toBytes)
                }
              } else {
                // Normal response
                ByteBuffer.wrap(response.toBytes)
              }
            } catch {
              case ex: Exception =>
                getPooledErrorResponse(StatusCode.InternalServerError)
            }
        }
    }
  }

  // Fixed: Simplified streaming response startup
  private def startStreamingResponse(key: SelectionKey, stream: Stream[IO[String]]): Unit = {
    virtualThreadExecutor.execute(() => {
      processStreamingResponse(key, stream)
    })
  }

  // Add missing method implementations
  private def processStreamingResponse(key: SelectionKey, stream: Stream[IO[String]]): Unit = {
    try {
      // Process the stream - this is a simplified implementation
      // You'll need to implement the actual streaming logic based on your Stream API
      stream.forEach { ioString =>
        ioString.map { data =>
          val sseData = s" $data\n\n"
          val buffer = ByteBuffer.wrap(sseData.getBytes())

          // Send the data chunk
          connectionStates.get(key) match {
            case Some(_) =>
              val connection = Connection(key.channel().asInstanceOf[SocketChannel])
              connection.write(buffer).unsafeRun()
            case None =>
              // Connection closed, stop streaming
              return
          }
        }.unsafeRun()
      }
    } catch {
      case ex: Exception =>
        // Handle streaming error
        cleanupConnectionUnsafe(key)
    }
  }

  private def createStreamingHeaders(response: HttpResponse[?]): ByteBuffer = {
    val headers = response.headers + (HeaderName("Transfer-Encoding") -> "chunked")
    val statusLine = s"HTTP/1.1 ${response.status.code} ${response.status.reasonPhrase}\r\n"
    val headerLines = headers.map { case (name, value) => s"${name.value}: $value\r\n" }.mkString
    val responseHeaders = statusLine + headerLines + "\r\n"
    ByteBuffer.wrap(responseHeaders.getBytes())
  }

  private def getPooledErrorResponse(statusCode: StatusCode): ByteBuffer = {
    statusCode match {
      case StatusCode.NotFound =>
        Option(notFound404Pool.poll()).getOrElse {
          val response = HttpResponse.notFound("Route not found")
          ByteBuffer.wrap(response.toBytes(using Codec.stringCodec))
        }
      case StatusCode.InternalServerError =>
        Option(error500Pool.poll()).getOrElse {
          val response = HttpResponse.internalError("Internal server error")
          ByteBuffer.wrap(response.toBytes(using Codec.stringCodec))
        }
      case StatusCode.BadRequest =>
        Option(badRequest400Pool.poll()).getOrElse {
          val response = HttpResponse.badRequest("Bad request")
          ByteBuffer.wrap(response.toBytes(using Codec.stringCodec))
        }
      case _ =>
        val response = HttpResponse.internalError("Internal server error")
        ByteBuffer.wrap(response.toBytes(using Codec.stringCodec))
    }
  }

  // Add missing methods
  private def handleAsyncResponse(key: SelectionKey, responseBytes: ByteBuffer): Unit = {
    connectionStates(key) = ConnectionState.Writing(responseBytes)
    pendingWrites.offer(key)

    if (needsWakeup.compareAndSet(false, true)) {
      //server.wakeupSelector()
    }
  }

  private def handleAsyncError(key: SelectionKey, ex: Exception): Unit = {
    val errorResponse = getPooledErrorResponse(StatusCode.InternalServerError)
    handleAsyncResponse(key, errorResponse)
  }

  private def handleWriteDirect(key: SelectionKey): Unit = {
    connectionStates.get(key) match {
      case Some(ConnectionState.Writing(buffer)) =>
        val connection = Connection(key.channel().asInstanceOf[SocketChannel])
        val bytesWritten = connection.write(buffer).unsafeRun()

        if (bytesWritten > 0 && !buffer.hasRemaining()) {
          // Write complete
          cleanupConnectionUnsafe(key)
        }
      case _ =>
        // Invalid state, cleanup
        cleanupConnectionUnsafe(key)
    }
  }

  private def cleanupConnectionUnsafe(key: SelectionKey): Unit = {
    connectionStates.get(key) match {
      case Some(ConnectionState.Reading(readBuffer, accBuffer)) =>
        releaseReadBuffer(readBuffer)
        releaseAccumulatedBuffer(accBuffer)
      case Some(ConnectionState.Processing(buffer)) =>
        releaseAccumulatedBuffer(buffer)
      case _ =>
    }

    connectionStates.remove(key)
    try {
      key.cancel()
      key.channel().close()
    } catch {
      case _: Exception => // Ignore cleanup errors
    }
  }
