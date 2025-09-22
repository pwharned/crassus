package org.pwharned.http.server

import org.pwharned.http.server.dsl.Handler
import org.pwharned.io.IO

import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.{ClosedChannelException, SelectionKey, Selector, ServerSocketChannel, SocketChannel}
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{ConcurrentLinkedQueue, ExecutorService, Executors}
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters.*
import scala.concurrent.ExecutionContext.Implicits.global


object  TcpServer  {
  final case class HttpAcceptEvent(
                                    key: SelectionKey,
                                    pools: BufferPoolCollection,
                                    // Handler now returns HttpResponse[?]
                                    handler: Handler
                                  ) extends AcceptEvent(key, pools) {

    @inline override def handleIO(): IO[Unit] = IO.effect {
      val server = key.channel().asInstanceOf[ServerSocketChannel]
      val client = server.accept()
      client.configureBlocking(false)

      val sk = client.register(key.selector(), SelectionKey.OP_READ)

      val state = HttpSessionState.newSession(
        client,
        sk,
        pools,
        handler // Pass the handler
      )

      sk.attach(state)
    }
  }


  // 2) ReadEvent disables OP_READ immediately, then launches the async work
  final case class HttpReadEvent(
                                  key: SelectionKey,
                                  selector: Selector,
                                  pendingOps: ConcurrentLinkedQueue[() => Unit],
                                  workerPool: ExecutorService
                                ) extends ReadEvent(key, selector, pendingOps, workerPool) {
    @inline override def handleIO(): IO[Unit] = IO.effect {
      // 1) stop further READ notifications for this key
      key.interestOps(key.interestOps() & ~SelectionKey.OP_READ)
      // 2) offload to your workerPool
      workerPool.submit(new Runnable {
        override def run(): Unit = {
          val state = key.attachment().asInstanceOf[HttpSessionState]
          state.onReadable()

          // 3) if the channel is still alive, re-enable READ and wake up selector
          if (state.channel.isOpen) {
            pendingOps.add(() => key.interestOps(key.interestOps() | SelectionKey.OP_READ))
          }
          if (wakeupNeeded.compareAndSet(false, true)) {
            selector.wakeup()
          }
        }
      })
    }


  }

  val bufferPools: BufferPoolCollection = Map(
    "small" -> BufferPool.heap(bufferSize = 1024),
    "large" -> BufferPool.direct(bufferSize = 8 * 1024)
  )
  private val wakeupNeeded = new AtomicBoolean(false)



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
      wakeupNeeded.set(false)

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
