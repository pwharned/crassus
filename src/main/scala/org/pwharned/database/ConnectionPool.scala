package org.pwharned.database

import java.sql.{Connection, DriverManager, SQLException}
import java.util.concurrent.TimeoutException
import scala.collection.mutable.Queue
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

class  ConnectionPool(
                          driverClassName: String,
                          jdbcUrl: String,
                          user: String,
                          password: String,
                          minPoolSize: Int = 5,
                          maxPoolSize: Int = 10000,
                          connectionTimeoutMillis: Long = 10000,
                          // New parameters for eviction
                          idleTimeoutMillis: Long = 30000,      // Connections idle longer than this are eligible for eviction.
                          evictionIntervalMillis: Long = 10000  // Frequency at which eviction occurs.
                        ):
  // Load the JDBC driver once during pool creation.
  Class.forName(driverClassName)

  // Internal state holds idle connections along with the timestamp (in millis) when they were returned.
  private val idleConnections: Queue[(Connection, Long)] = Queue.empty
  private var totalConnections: Int = 0

  // Eviction thread control flag.
  @volatile private var evictionActive = true

  // Start a background eviction thread.
  private val evictionThread: Thread = new Thread(() => {
    try {
      while (evictionActive) {
        Thread.sleep(evictionIntervalMillis)
        evictIdleConnections()
      }
    } catch {
      case _: InterruptedException => // Thread was interrupted; exit.
    }
  })
  evictionThread.setDaemon(true)
  evictionThread.start()

  // Prepopulate the pool with the minimum number of connections.
  for (_ <- 1 to minPoolSize) do
    val conn = createConnection().map{
      c => idleConnections.enqueue((c, System.currentTimeMillis()))
      totalConnections += 1
    }


  // Helper method to create a new JDBC connection.
  private def createConnection(): Try[Connection] = Try{DriverManager.getConnection(jdbcUrl, user, password)}

  /**
   * Acquires a connection from the pool.
   *
   * If an idle connection is available, it is checked for expiration before returning.
   * If none is available and we're below maxPoolSize, a new connection is created.
   * Otherwise, the caller waits until a connection becomes available or a timeout occurs.
   */

  def acquire(): Try[Connection] = this.synchronized {
    val startTime = System.currentTimeMillis()
    var now = startTime

    while (idleConnections.isEmpty && totalConnections >= maxPoolSize) do {
      val elapsed = now - startTime
      val remaining = connectionTimeoutMillis - elapsed
      if (remaining <= 0) return Failure(new TimeoutException("Timeout waiting for a JDBC connection from the pool."))

      wait(remaining)
      now = System.currentTimeMillis()
    }

    if (idleConnections.nonEmpty) then {
      val (conn, returnedTime) = idleConnections.dequeue()
      if (now - returnedTime > idleTimeoutMillis) then {
        Try(conn.close()).recover { case e: SQLException => println(s"Error closing expired connection: ${e.getMessage}") }
        totalConnections -= 1
        acquire()
      } else Success(conn)
    } else {
      totalConnections += 1
      createConnection()
    }
  }


  /**
   * Releases a connection back to the pool.
   *
   * If the connection is closed or invalid, it is not reused.
   * Otherwise, the connection is enqueued along with the current timestamp.
   */
  def release(conn: Connection): Unit = this.synchronized {
    try
      if (conn.isClosed || !conn.isValid(2)) then
        totalConnections -= 1
      else
        idleConnections.enqueue((conn, System.currentTimeMillis()))
    catch
      case _: SQLException => totalConnections -= 1
    notifyAll() // Signal waiting threads that a connection may be available.
  }

  /**
   * Evicts idle connections that have been in the pool longer than idleTimeoutMillis.
   *
   * This method ensures that the pool does not keep more connections than are needed
   * after periods of high load, while preserving at least minPoolSize connections.
   */
  private def evictIdleConnections(): Unit = this.synchronized {
    val now = System.currentTimeMillis()
    // Since idleConnections is FIFO, we can check the head until we find a connection within timeout.
    while (idleConnections.nonEmpty && totalConnections > minPoolSize) {
      val (conn, returnedTime) = idleConnections.head
      if (now - returnedTime > idleTimeoutMillis) {
        idleConnections.dequeue()
        try conn.close() catch { case _: SQLException => () }
        totalConnections -= 1
      } else {
        // Since the oldest connection is still valid (idle-wise), later ones must be newer.
        return
      }
    }
    notifyAll()
  }

  /**
   * Provides a functional “loan” method that automatically acquires a connection,
   * passes it to the provided function, and ensures that the connection is returned
   * to the pool after use.
   */
  def withConnection[T](f: Connection => T)(using ec: scala.concurrent.ExecutionContext): Future[Try[T]] = {
    Future(acquire()) .map {
      case Failure(exception) => Failure(exception)
      case con@Success(connection) =>
        Try(f(connection)).transform(
          result => {
            release(connection); Success(result)
          },
          error => {
            release(connection); Failure(error)
          }
        )
    }
  }


  /**
   * Shuts down the pool by stopping the eviction thread and closing all idle connections.
   *
   * This should be called during application shutdown to clean up resources.
   */
  def shutdown(): Unit = this.synchronized {
    evictionActive = false
    evictionThread.interrupt()
    while (idleConnections.nonEmpty) do
      val (conn, _) = idleConnections.dequeue()
      try conn.close() catch { case _: SQLException => () }
      totalConnections -= 1
  }