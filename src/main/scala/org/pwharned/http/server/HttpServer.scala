package org.pwharned.http.server
import org.pwharned.codec.Codec
import org.pwharned.http.HttpTypes.HttpPath
import org.pwharned.http.*
import org.pwharned.io.IO
import java.nio.ByteBuffer
import java.nio.channels.{SelectionKey, SocketChannel}
import java.util.concurrent.{Executors, ConcurrentLinkedQueue, ConcurrentHashMap}
import java.util.concurrent.atomic.{AtomicInteger, AtomicBoolean}
import scala.collection.mutable
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
          checkForCompleteRequestDirect(key, accumulated) // Fixed: was calling processRequestAsyncDirect directly
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
        val responseBytes = processRequestInVirtualThread(buffer)
        handleAsyncResponse(key, responseBytes)
      } catch {
        case ex: Exception =>
          handleAsyncError(key, ex)
      }
    })
  }
  private def processRequestInVirtualThread(buffer: ByteBuffer): ByteBuffer = {
    try {
      HttpRequest.parseRawRequest(buffer) match {
        case Left(error) =>
          getPooledErrorResponse(StatusCode.BadRequest)
        case Right((method, path)) =>
          processTypedRequestInVirtualThread(buffer, method, path)
      }
    } catch {
      case ex: Exception =>
        getPooledErrorResponse(StatusCode.InternalServerError)
    }
  }
  private def processTypedRequestInVirtualThread(buffer: ByteBuffer, method: HttpMethod, path: HttpPath): ByteBuffer = {
    findRouteFast(method, path) match {
      case None =>
        getPooledErrorResponse(StatusCode.NotFound)
      case Some(route) =>
        handleTypedRouteInVirtualThread(buffer, route)
    }
  }
  private def handleTypedRouteInVirtualThread(buffer: ByteBuffer, route: Route[?, ?]): ByteBuffer = {
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
              ByteBuffer.wrap(response.toBytes)
            } catch {
              case ex: Exception =>
                getPooledErrorResponse(StatusCode.InternalServerError)
            }
        }
    }
  }
  private def getPooledErrorResponse(status: StatusCode): ByteBuffer = {
    val pooled = status match {
      case StatusCode.NotFound => notFound404Pool.poll()
      case StatusCode.BadRequest => badRequest400Pool.poll()
      case StatusCode.InternalServerError => error500Pool.poll()
      case _ => null
    }
    if (pooled != null) {
      pooled.rewind()
      pooled
    } else {
      createErrorResponseBytes(status, status.reasonPhrase)
    }
  }
  private def createErrorResponseBytes(status: StatusCode, message: String): ByteBuffer = {
    val response = HttpResponse.internalError(message).withStatus(status)
    ByteBuffer.wrap(response.toBytes(using Codec.stringCodec))
  }
  // Optimized async response handling with batched wakeups
  private def handleAsyncResponse(key: SelectionKey, responseBytes: ByteBuffer): Unit = {
    try {
      synchronized {
        connectionStates.get(key) match {
          case Some(ConnectionState.Processing(_)) =>
            connectionStates(key) = ConnectionState.Writing(responseBytes)
            key.interestOps(SelectionKey.OP_WRITE)

            // Queue for batched processing instead of immediate wakeup
            pendingWrites.offer(key)

            // Only wakeup if we're the first pending write
            if (needsWakeup.compareAndSet(false, true)) {
              key.selector().wakeup()
            }
          case _ =>
        }
      }
    } catch {
      case ex: Exception =>
        cleanupConnectionUnsafe(key)
    }
  }
  private def handleAsyncError(key: SelectionKey, exception: Throwable): Unit = {
    try {
      val errorResponse = getPooledErrorResponse(StatusCode.InternalServerError)
      handleAsyncResponse(key, errorResponse)
    } catch {
      case ex: Exception =>
        cleanupConnectionUnsafe(key)
    }
  }
  private def handleWriteDirect(key: SelectionKey): Unit = {
    connectionStates.get(key) match {
      case Some(ConnectionState.Writing(buffer)) =>
        val connection = Connection(key.channel().asInstanceOf[SocketChannel])
        val bytesWritten = connection.write(buffer).unsafeRun()
        if (!buffer.hasRemaining) {
          cleanupConnectionUnsafe(key)
        }
      case _ =>
        cleanupConnectionUnsafe(key)
    }
  }
  private def cleanupConnectionUnsafe(key: SelectionKey): Unit = {
    // Return buffers to pools before removing connection
    connectionStates.get(key) match {
      case Some(ConnectionState.Reading(readBuf, accBuf)) =>
        releaseReadBuffer(readBuf)
        releaseAccumulatedBuffer(accBuf)
      case Some(ConnectionState.Processing(buf)) =>
        // Determine buffer type by capacity and return to appropriate pool
        if (buf.capacity() == 8192) releaseReadBuffer(buf)
        else if (buf.capacity() == 16384) releaseAccumulatedBuffer(buf)
      case Some(ConnectionState.Writing(_)) =>
      // Writing buffer is a response buffer (don't pool)
      case _ =>
    }

    val sizeBefore = connectionStates.size
    connectionStates.remove(key)


    key.cancel()
    key.channel().close()
  }

  // Legacy IO methods for compatibility
  private def handleServerEvent(event: ServerEvent): IO[Unit] = IO.effect {
    handleServerEventDirect(event)
  }

  private def handleRead(key: SelectionKey): IO[Unit] = IO.effect {
    handleReadDirect(key)
  }

