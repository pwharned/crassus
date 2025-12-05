package org.pwharned.http.server.tcp

import java.net.InetSocketAddress
import java.nio.channels.{SelectionKey, Selector, ServerSocketChannel, SocketChannel}
import java.util.concurrent.{ConcurrentLinkedQueue, ExecutorService, Executors}
import scala.jdk.CollectionConverters.*

/**
 * Generic NIO-based TCP server.
 * Type-parameterized over request/response types.
 *
 * Performance characteristics:
 * - Non-blocking NIO with selector
 * - Buffer pooling via BufferAllocator
 * - Virtual threads for request processing
 * - Batch event processing
 * - Zero-copy where possible
 */
class TcpServer[Req](port: Int)(using proto: Protocol[Req]):
  // Core NIO components
  private given selector: Selector = Selector.open()
  private given alloc: BufferAllocator = BufferAllocator.direct(proto.bufferSize)

  // Async execution
  private val executor: ExecutorService = Executors.newVirtualThreadPerTaskExecutor()
  private val pendingOps = new ConcurrentLinkedQueue[() => Unit]()

  private val serverChannel = ServerSocketChannel.open()

  /**
   * Create new session for accepted connection.
   * Allocates buffers from pool.
   */
  private inline def createSession(channel: SocketChannel, key: SelectionKey): Session[Req] =
    Session(
      channel = channel,
      key = key,
      readBuffer = alloc.allocateRead(),
      writeBuffer = alloc.allocateWrite(),
      parser = proto.parser
    )

  /**
   * Handle read event for a session.
   * Feeds parser, handles request, renders response.
   */

  private inline def handleRead(session: Session[Req]): Unit =


    session.readBuffer.clear()
    val bytesRead = session.channel.read(session.readBuffer)

    if bytesRead == -1 then
      proto.onConnectionClose(session.channel)
      session.close()
    else if bytesRead > 0 then
      session.readBuffer.flip()
      session.parser.feed(session.readBuffer)

      session.parser.take().foreach { request =>
        // Simple: just call the handler with the request and buffers
        proto.handler.handle(request, session.writeBuffer, session.channel)
      }


  /**
   * Main event loop.
   * Processes accept/read events, offloads work to executor.
   */
  def start(): Unit =
    serverChannel.configureBlocking(false)
    serverChannel.bind(new InetSocketAddress(port))
    serverChannel.register(selector, SelectionKey.OP_ACCEPT)

    println(s"Server listening on port $port")

    while true do
      // Process pending selector ops (from worker threads)
      var op = pendingOps.poll()
      while op != null do
        op()
        op = pendingOps.poll()

      // Select ready channels
      if selector.select() > 0 then
        val keys = selector.selectedKeys().iterator().asScala
        var count = 0

        // Batch process up to maxBatch events
        while keys.hasNext && count < proto.maxBatch do
          val key = keys.next()

          if key.isValid then
            // ACCEPT - new connection
            if key.isAcceptable then
              val server = key.channel().asInstanceOf[ServerSocketChannel]
              val client = server.accept()

              if client != null then
                client.configureBlocking(false)
                val clientKey = client.register(selector, SelectionKey.OP_READ)
                val session = createSession(client, clientKey)
                clientKey.attach(session)
                proto.onConnectionOpen(client)

            // READ - data available
            else if key.isReadable then

              // Disable READ while processing (level-triggered)
              key.interestOps(key.interestOps() & ~SelectionKey.OP_READ)

              // Offload to worker thread
              executor.submit(new Runnable {
                def run(): Unit =
                  val session = key.attachment().asInstanceOf[Session[Req]]

                  try
                    handleRead(session)

                    // Re-enable READ if still open
                    if session.isOpen then
                      pendingOps.add(() =>
                        key.interestOps(key.interestOps() | SelectionKey.OP_READ)
                      )
                      selector.wakeup()
                  catch
                    case e: Exception =>
                      e.printStackTrace()
                      proto.onConnectionClose(session.channel)
                      session.close()
              })

          count += 1

        selector.selectedKeys().clear()

    // Cleanup on shutdown
    sys.addShutdownHook {
      executor.shutdown()
      selector.close()
      serverChannel.close()
    }
