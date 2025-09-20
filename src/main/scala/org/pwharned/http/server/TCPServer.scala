scala
import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.{ServerSocketChannel, SocketChannel, SelectionKey, Selector}
import java.util.concurrent.{ConcurrentLinkedQueue, Executors}
import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.JavaConverters._
import scala.util.{Try, Success, Failure}
import scala.concurrent.{Future, ExecutionContext}
import scala.util.control.NonFatal

// --- 1. Connection Abstraction ---

/**
 * Represents a client connection. Provides methods to read and write bytes.
 * This is the interface that connection handlers will interact with.
 */
trait Connection {
  def remoteAddress: InetSocketAddress

  /** Reads available bytes from the connection into the given buffer. */
  def read(buffer: ByteBuffer): Try[Int]

  /** Writes bytes from the given buffer to the connection. */
  def write(buffer: ByteBuffer): Try[Int]

  /** Closes the connection. */
  def close(): Unit
}

// --- 2. Connection Handler Type ---

/**
 * A type alias for our connection handler.
 * It's a function that takes a Connection and an ExecutionContext,
 * and returns a Future[Unit] representing the completion of handling.
 * The Future allows asynchronous processing.
 */
type ConnectionHandler = (Connection, ExecutionContext) => Future[Unit]

// --- 3. Internal Connection Implementation (managed by the server) ---

/**
 * Internal representation of a client connection, managed by the server.
 * This holds the low-level NIO channel and the read/write buffers.
 * It also manages a queue for outgoing data when writes are not immediately possible.
 *
 * @param channel The underlying SocketChannel.
 * @param readBuffer The buffer used for reading data from the channel.
 * @param writeBuffer The buffer used for writing data to the channel (intermediate for server's own writes).
 * @param selector The selector this connection is registered with.
 */
private class NioConnection(
                             val channel: SocketChannel,
                             private val readBuffer: ByteBuffer, // Attachment for the SelectionKey
                             private val writeBuffer: ByteBuffer, // For server's own internal use to prepare data
                             private val selector: Selector
                           ) extends Connection {

  private val writeQueue = new ConcurrentLinkedQueue[ByteBuffer]()
  private val closing = new AtomicBoolean(false)

  override def remoteAddress: InetSocketAddress = channel.getRemoteAddress.asInstanceOf[InetSocketAddress]

  override def read(buffer: ByteBuffer): Try[Int] = Try {
    // We expect the 'buffer' here to be the one attached to the key, already flipped if needed
    // The handler will read from this buffer.
    // However, for this `read` method, we are simulating a direct read from the channel
    // for the handler to pull data.
    // In a true streaming model, the server would push data to the handler.
    // For now, let's use the attached buffer.
    buffer.clear() // Clear for next read from channel
    val bytesRead = channel.read(buffer)
    if (bytesRead > 0) buffer.flip() // Prepare for handler to read
    bytesRead
  }

  // Enqueues data to be written. This is asynchronous.
  override def write(buffer: ByteBuffer): Try[Int] = Try {
    if (closing.get()) throw new IllegalStateException("Connection is closing.")

    if (buffer.hasRemaining) {
      // Try to write directly first
      val bytesWritten = channel.write(buffer)
      if (buffer.hasRemaining) {
        // Not all bytes written, enqueue the remainder
        // Make a copy to avoid modification issues
        val remainingBuffer = ByteBuffer.allocateDirect(buffer.remaining())
        remainingBuffer.put(buffer)
        remainingBuffer.flip()
        writeQueue.offer(remainingBuffer)
        // Ensure OP_WRITE is registered if not already
        val key = channel.keyFor(selector)
        if (key != null && (key.interestOps() & SelectionKey.OP_WRITE) == 0) {
          key.interestOps(key.interestOps() | SelectionKey.OP_WRITE)
          selector.wakeup() // Wake up the selector to process the change
        }
      }
      bytesWritten // Return bytes written immediately
    } else {
      0 // Nothing to write
    }
  }

  // Internal method for the server to drain the write queue
  def drainWriteQueue(): Unit = {
    if (closing.get()) return

    val key = channel.keyFor(selector)
    if (key == null || !key.isValid) {
      close()
      return
    }

    var bytesWrittenTotal = 0
    var bufferToWrite: ByteBuffer = writeQueue.peek()
    while (bufferToWrite != null) {
      try {
        val bytesWritten = channel.write(bufferToWrite)
        bytesWrittenTotal += bytesWritten
        if (!bufferToWrite.hasRemaining) {
          // Entire buffer written, remove from queue
          writeQueue.poll()
        } else {
          // Buffer partially written, stop and wait for next OP_WRITE
          // The selector will keep notifying us for OP_WRITE until the queue is empty
          bufferToWrite = null // Exit loop
        }
      } catch {
        case NonFatal(e) =>
          println(s"Error draining write queue for ${remoteAddress}: ${e.getMessage}")
          close()
          return
      }
      bufferToWrite = writeQueue.peek() // Get next buffer if current one was fully written
    }

    // If queue is empty, remove OP_WRITE interest to avoid busy-looping
    if (writeQueue.isEmpty) {
      key.interestOps(key.interestOps() & ~SelectionKey.OP_WRITE)
    }
  }


  override def close(): Unit = {
    if (closing.compareAndSet(false, true)) {
      println(s"Closing connection to: ${remoteAddress}")
      Try(channel.close()) match {
        case Failure(e) => println(s"Error closing channel: ${e.getMessage}")
        case _ => // Success
      }
      // Clean up resources, e.g., if buffers were pooled, return them
      val key = channel.keyFor(selector)
      if (key != null) key.cancel()
    }
  }
}

// --- 4. The Server Itself ---

object StreamingTcpServer {
  val PORT = 8080
  val READ_BUFFER_SIZE = 4096 // Larger buffer for more efficient reads
  val WRITE_BUFFER_SIZE = 4096 // For internal server use if needed

  // Use a fixed thread pool for handling connection logic.
  // This offloads potentially blocking or long-running logic from the NIO thread.
  implicit val connectionExecutionContext: ExecutionContext = ExecutionContext.fromExecutor(
    Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors() * 2)
  )

  def start(handler: ConnectionHandler): Unit = {
    println(s"Starting Scala 3 Streaming TCP Server on port $PORT...")

    val selector = Selector.open()
    val serverSocketChannel = ServerSocketChannel.open()
    serverSocketChannel.bind(new InetSocketAddress("localhost", PORT))
    serverSocketChannel.configureBlocking(false)
    serverSocketChannel.register(selector, SelectionKey.OP_ACCEPT)

    println("Server ready to accept connections.")

    try {
      while (true) {
        selector.select() // Blocks until an event occurs

        val selectedKeys = selector.selectedKeys().iterator().asScala
        while (selectedKeys.hasNext) {
          val key = selectedKeys.next()

          if (!key.isValid) {
            // Key might have been invalidated during processing, e.g., connection closed.
            Option(key.attachment()).foreach {
              case conn: NioConnection => conn.close()
              case _ => // Nothing to do
            }
            key.cancel()
            continue
          }

          if (key.isAcceptable) {
            val clientChannel = serverSocketChannel.accept()
            clientChannel.configureBlocking(false)
            val readBuffer = ByteBuffer.allocateDirect(READ_BUFFER_SIZE)
            val writeBuffer = ByteBuffer.allocateDirect(WRITE_BUFFER_SIZE) // For internal server usage if any
            val nioConnection = new NioConnection(clientChannel, readBuffer, writeBuffer, selector)
            clientChannel.register(selector, SelectionKey.OP_READ, nioConnection) // Attach the NioConnection instance

            println(s"Accepted connection from: ${nioConnection.remoteAddress}")

            // Hand off the connection to the user-defined handler in a Future
            Future {
              handler(nioConnection, connectionExecutionContext)
            }.flatten.onComplete {
              case Success(_) => println(s"Handler for ${nioConnection.remoteAddress} completed gracefully.")
              case Failure(e) => println(s"Handler for ${nioConnection.remoteAddress} failed: ${e.getMessage}")
            }(connectionExecutionContext) // Use the same EC for onComplete
          } else if (key.isReadable) {
            val nioConnection = key.attachment().asInstanceOf[NioConnection]
            val clientChannel = nioConnection.channel
            val readBuffer = nioConnection.readBuffer // This is the buffer attached during registration

            // Clear buffer before reading new data
            readBuffer.clear()

            try {
              val bytesRead = clientChannel.read(readBuffer)
              if (bytesRead == -1) {
                // Client closed the connection gracefully
                nioConnection.close()
              } else if (bytesRead > 0) {
                readBuffer.flip() // Prepare buffer for handler to consume

                // The handler will conceptually take this data.
                // In a true stream, the handler would subscribe to a stream of buffers.
                // For now, we'll let the handler's read method manage its own data pull.
                // The key is that the data is *available* here.
              }
            } catch {
              case NonFatal(e) =>
                println(s"Error reading from client ${nioConnection.remoteAddress}: ${e.getMessage}")
                nioConnection.close()
            }
          } else if (key.isWritable) {
            val nioConnection = key.attachment().asInstanceOf[NioConnection]
            nioConnection.drainWriteQueue() // Attempt to write pending data
          }
        }
      }
    } catch {
      case NonFatal(e) => println(s"Server crashed: ${e.getMessage}")
    } finally {
      Try(serverSocketChannel.close())
      Try(selector.close())
      connectionExecutionContext match {
        case ec: ExecutionContextExecutorService => ec.shutdown()
        case _ => // Cannot shut down, e.g., if it's a global EC
      }
    }
  }

  // To allow graceful shutdown of the execution context
  implicit class ExecutionContextExecutorService(val ec: ExecutionContext) {
    def shutdown(): Unit = ec match {
      case pool: java.util.concurrent.ExecutorService => pool.shutdown()
      case _ => // Do nothing
    }
  }
}

// --- 5. Example Connection Handler (Ping-Pong) ---

object PingPongHandler {
  val PING_MESSAGE = "PING!\n"
  val PONG_MESSAGE = "PONG!\n"
  val UNKNOWN_MESSAGE = "Received: %s. Please send PING!\n"

  def apply(connection: Connection, ec: ExecutionContext): Future[Unit] = Future {
    println(s"Handler started for ${connection.remoteAddress}")
    val incomingBuffer = ByteBuffer.allocateDirect(1024) // Handler's own buffer for processing

    // Simulate continuous reading and writing
    while (!Thread.currentThread().isInterrupted && connection.read(incomingBuffer).map(_ != -1).getOrElse(false)) {
      incomingBuffer.flip() // Prepare to read from the buffer

      if (incomingBuffer.hasRemaining) {
        val bytes = new Array[Byte](incomingBuffer.remaining())
        incomingBuffer.get(bytes)
        val message = new String(bytes).trim

        println(s"Handler received from ${connection.remoteAddress}: $message")

        val responseBuffer = if (message.equalsIgnoreCase("PING!")) {
          ByteBuffer.wrap(PONG_MESSAGE.getBytes())
        } else {
          ByteBuffer.wrap(UNKNOWN_MESSAGE.format(message).getBytes())
        }

        connection.write(responseBuffer) match {
          case Success(bytesWritten) => println(s"Handler wrote $bytesWritten bytes to ${connection.remoteAddress}")
          case Failure(e) =>
            println(s"Handler write error to ${connection.remoteAddress}: ${e.getMessage}")
            connection.close()
            return // Exit handler loop
        }
      }
      incomingBuffer.clear() // Clear for next read
      // In a real streaming scenario, this would be a reactive pull/push
      // For now, we loop and 'pull' by calling read.
      Thread.sleep(10) // Small delay to prevent busy looping in this simplified model
    }
    println(s"Handler finished for ${connection.remoteAddress}")
    connection.close() // Ensure connection is closed when handler finishes
  }(ec)
}

// --- 6. Main entry point to start the server ---

@main def runServer(): Unit = {
  StreamingTcpServer.start(PingPongHandler.apply)
}
