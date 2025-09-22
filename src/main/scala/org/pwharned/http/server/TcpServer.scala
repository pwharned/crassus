package org.pwharned.http.server

import org.pwharned.io.IO

import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.{ClosedChannelException, SelectionKey, Selector, ServerSocketChannel, SocketChannel}
import java.util.concurrent.{ConcurrentLinkedQueue, ExecutorService, Executors}
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters.*
import scala.concurrent.ExecutionContext.Implicits.global


object  TcpServer  {


  val bufferPools: BufferPoolCollection = Map(
    "small" -> BufferPool.heap(bufferSize = 1024),
    "large" -> BufferPool.direct(bufferSize = 8 * 1024)
  )




  // a fixed pool (or virtual‐thread executor) for your async tasks
  val workerPool: ExecutorService = Executors.newVirtualThreadPerTaskExecutor()

  // queue for selector‐thread interestOps changes
  val pendingOps = new ConcurrentLinkedQueue[() => Unit]()

  private val MaxBatch     = 128
  val buf: ArrayBuffer[IOEvent] = ArrayBuffer.empty[IOEvent]

  // The acceptFactory now needs to produce HttpAcceptEvent, not just AcceptEvent
  inline def run[A<:AcceptEvent,
    B<:ReadEvent](port:Int, acceptFactory: (SelectionKey, BufferPoolCollection) => A, // Removed default value
                  readFactory:   (SelectionKey, Selector,ConcurrentLinkedQueue[() => Unit],
                    ExecutorService)    => B = HttpReadEvent.apply): Unit = // Changed default to HttpReadEvent
    val selector: Selector = Selector.open()
    val serverChannel = ServerSocketChannel.open()
    serverChannel.bind(new InetSocketAddress(port))
    serverChannel.configureBlocking(false)
    serverChannel.register(selector, SelectionKey.OP_ACCEPT)
    println(s"Server listening on port $port") // Use the actual port
    val selectedKeys = selector.selectedKeys()


    while (true) {
      // ─── Drain any selector‐thread tasks  ─────────────────
      var task = pendingOps.poll()
      while (task != null) {
        task()
        task = pendingOps.poll()
      }

      // ─── Select and batch events ──────────────────────────
      buf.clear()
      var cnt = 0

      if (selector.select() > 0) {
        val it = selectedKeys.iterator().asScala
        while (it.hasNext && cnt < MaxBatch) {
          val key = it.next()
          if (key.isAcceptable)     buf += acceptFactory(key, bufferPools)
          else if (key.isReadable)  buf += readFactory(key, selector, pendingOps, workerPool)
          cnt+=1
        }
        selectedKeys.clear()
      }
      buf.iterator
        .take(cnt)
        .foreach(ev => {
          ev.handleIO().unsafeRunOptimized()
        })

    }

    sys.addShutdownHook {
      workerPool.shutdown()
    }
}
