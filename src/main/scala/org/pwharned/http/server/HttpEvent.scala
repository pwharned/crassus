package org.pwharned.http.server

import org.pwharned.http.request.HttpRequestView
import org.pwharned.http.response.HttpResponse
import org.pwharned.http.server.dsl.Handler
import org.pwharned.io.IO

import java.nio.channels.{SelectionKey, Selector, ServerSocketChannel}
import java.util.concurrent.{ConcurrentLinkedQueue, ExecutorService}



final case class HttpAcceptEvent(
                                  key:         SelectionKey,
                                  pools:       BufferPoolCollection,
                                  // Handler now returns HttpResponse[?]
                                  handler:    Handler
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
                    ) extends ReadEvent(key, selector,pendingOps, workerPool) {
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
          selector.wakeup()
        }
      }
    })
  }

  
}