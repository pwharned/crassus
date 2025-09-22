package org.pwharned.http.server

import java.nio.ByteBuffer
import scala.collection.mutable

type BufferPoolCollection = Map[String, BufferPool]

trait BufferPool {
  def allocateReadBuffer(): ByteBuffer

  def allocateWriteBuffer(): ByteBuffer
}


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

  def direct(bufferSize: Int): BufferPool = new BufferPool {
    override def allocateReadBuffer(): ByteBuffer = ByteBuffer.allocateDirect(bufferSize)

    override def allocateWriteBuffer(): ByteBuffer = ByteBuffer.allocateDirect(bufferSize)
  }

  def heap(bufferSize: Int): BufferPool = new BufferPool {
    override def allocateReadBuffer(): ByteBuffer = ByteBuffer.allocate(bufferSize)

    override def allocateWriteBuffer(): ByteBuffer = ByteBuffer.allocate(bufferSize)
  }

  def pooled(bufferSize: Int, poolSize: Int): BufferPool = new BufferPool {
    private val readBufs = Array.fill(poolSize)(ByteBuffer.allocateDirect(bufferSize))
    private val writeBufs = Array.fill(poolSize)(ByteBuffer.allocateDirect(bufferSize))
    private var idx = 0

    private def nextIndex(): Int = {
      val i = idx;
      idx = (idx + 1) % poolSize;
      i
    }

    override def allocateReadBuffer(): ByteBuffer = {
      val b = readBufs(nextIndex());
      b.clear();
      b
    }

    override def allocateWriteBuffer(): ByteBuffer = {
      val b = writeBufs(nextIndex());
      b.clear();
      b
    }
  }