package org.pwharned.http

import java.nio.ByteBuffer
import scala.collection.mutable

object BufferPool:
  private val pool = mutable.Queue[ByteBuffer]()
  private val maxPoolSize = 100
  private val bufferSize = 8192

  def acquire(): ByteBuffer = synchronized {
    if pool.nonEmpty then
      val buffer = pool.dequeue()
      buffer.clear()
      buffer
    else
      ByteBuffer.allocate(bufferSize)
  }

  def release(buffer: ByteBuffer): Unit = synchronized {
    if pool.size < maxPoolSize then
      buffer.clear()
      pool.enqueue(buffer)
  }