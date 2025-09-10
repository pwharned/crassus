package org.pwharned.http.server

import org.pwharned.io.IO
import org.pwharned.stream.Stream

import java.net.InetSocketAddress
import java.nio.channels.{SelectionKey, Selector, ServerSocketChannel}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

class Server(port: Int):
  private val serverChannel = ServerSocketChannel.open()
  private val selector = Selector.open()
  private var serverKey: SelectionKey = _

  def bind(): IO[Unit] = IO.effect {
    serverChannel.bind(InetSocketAddress(port))
    serverChannel.configureBlocking(false)
    serverKey = serverChannel.register(selector, SelectionKey.OP_ACCEPT)
  }

  // Legacy method for compatibility
  def accept(): IO[Option[Connection]] = IO.effect {
    Option(serverChannel.accept()).map(Connection(_))
  }

  // NEW: Optimized method that processes ALL events from single select() call
  def waitForEventsMultiple(): List[ServerEvent] = {
    // Single select call - this was the bottleneck!
    selector.select()

    val events = mutable.ListBuffer[ServerEvent]()
    val keys = selector.selectedKeys().asScala.toList
    selector.selectedKeys().clear()

    // Process ALL ready keys in one go instead of just the first
    keys.foreach { key =>
      if (key.isValid) {
        if (key.isAcceptable) {
          // Handle new connections
          val clientChannel = serverChannel.accept()
          if (clientChannel != null) {
            clientChannel.configureBlocking(false)
            val readKey = clientChannel.register(selector, SelectionKey.OP_READ)
            events += ServerEvent.NewConnection(Connection(clientChannel), readKey)
          }
        } else if (key.isReadable) {
          events += ServerEvent.DataReady(key)
        } else if (key.isWritable) {
          events += ServerEvent.WriteReady(key)
        }
      }
    }

    // Return all events, or NoEvent if nothing was ready
    if (events.nonEmpty) events.toList else List(ServerEvent.NoEvent)
  }

  // Original single-event method (keep for compatibility)
  def waitForEvents(): IO[ServerEvent] =
    IO.effect {
      selector.select() // Block until events are ready
      val keys = selector.selectedKeys().asScala.toList
      selector.selectedKeys().clear()

      // Process the first ready key and return appropriate event
      keys.find(_.isValid) match {
        case Some(key) if key.isAcceptable =>
          val clientChannel = serverChannel.accept()
          if (clientChannel != null) {
            clientChannel.configureBlocking(false)
            val readKey = clientChannel.register(selector, SelectionKey.OP_READ)
            ServerEvent.NewConnection(Connection(clientChannel), readKey)
          } else {
            ServerEvent.NoEvent
          }
        case Some(key) if key.isReadable =>
          ServerEvent.DataReady(key)
        case Some(key) if key.isWritable =>
          ServerEvent.WriteReady(key)
        case _ =>
          ServerEvent.NoEvent
      }
    }

  // Alternative: Optimized version with timeout to prevent spinning
  def waitForEventsMultipleWithTimeout(timeoutMs: Long = 1): List[ServerEvent] = {
    val readyChannels = selector.select(timeoutMs)

    if (readyChannels == 0) {
      // No events ready, return empty list to prevent spinning
      return List(ServerEvent.NoEvent)
    }

    val events = mutable.ListBuffer[ServerEvent]()
    val keys = selector.selectedKeys().asScala.toList
    selector.selectedKeys().clear()

    keys.foreach { key =>
      if (key.isValid) {
        if (key.isAcceptable) {
          val clientChannel = serverChannel.accept()
          if (clientChannel != null) {
            clientChannel.configureBlocking(false)
            val readKey = clientChannel.register(selector, SelectionKey.OP_READ)
            events += ServerEvent.NewConnection(Connection(clientChannel), readKey)
          }
        } else if (key.isReadable) {
          events += ServerEvent.DataReady(key)
        } else if (key.isWritable) {
          events += ServerEvent.WriteReady(key)
        }
      }
    }

    if (events.nonEmpty) events.toList else List(ServerEvent.NoEvent)
  }

  // New event-driven stream that emits ServerEvents
  def eventStream(): Stream[IO[ServerEvent]] =
    Stream.unfold(()) { _ =>
      Some((waitForEvents(), ()))
    }

  // Legacy compatibility method
  def acceptStream(): Stream[IO[Connection]] =
    eventStream()
      .map(_.map {
        case ServerEvent.NewConnection(conn, _) => conn
        case _ => throw new RuntimeException("Expected new connection")
      })
      .filter(_ => true)

  def close(): IO[Unit] = IO.effect {
    selector.close()
    serverChannel.close()
  }

// Event types for the server
sealed trait ServerEvent
object ServerEvent:
  case class NewConnection(connection: Connection, key: SelectionKey) extends ServerEvent
  case class DataReady(key: SelectionKey) extends ServerEvent
  case class WriteReady(key: SelectionKey) extends ServerEvent
  case object NoEvent extends ServerEvent
