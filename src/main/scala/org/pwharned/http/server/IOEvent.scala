package org.pwharned.http.server

import org.pwharned.io.IO

import java.nio.channels.{SelectionKey, Selector, ServerSocketChannel}
import java.util.concurrent.{ConcurrentLinkedQueue, ExecutorService}
import scala.concurrent.{ExecutionContext, Future}

sealed trait IOEvent {
  @inline def handleIO(): IO[Unit]
}

// 2) Wrap your existing logic in IO.effect
class AcceptEvent(key: SelectionKey, pool: BufferPoolCollection) extends IOEvent {
  @inline def handleIO(): IO[Unit] = IO.effect {
    val server = key.channel().asInstanceOf[ServerSocketChannel]
    val client = server.accept()
    client.configureBlocking(false)
    val sk    = client.register(key.selector(), SelectionKey.OP_READ)
    val state = SessionState.newSession(client, sk, pool)
    sk.attach(state)
  }
}

// 2) ReadEvent disables OP_READ immediately, then launches the async work
class ReadEvent(
                      key: SelectionKey,
                      selector: Selector,
                      pendingOps: ConcurrentLinkedQueue[() => Unit],
                      workerPool: ExecutorService
                    ) extends IOEvent {
  @inline def handleIO(): IO[Unit] = IO.effect {
    // 1) stop further READ notifications for this key
    key.interestOps(key.interestOps() & ~SelectionKey.OP_READ)
    workerPool.submit(new Runnable {
      override def run(): Unit = {
        val state = key.attachment().asInstanceOf[SessionState]
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