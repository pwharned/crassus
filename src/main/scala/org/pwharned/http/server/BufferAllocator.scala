package org.pwharned.http.server.tcp

import java.nio.ByteBuffer
import java.util.concurrent.ConcurrentLinkedQueue

/** Type class for buffer allocation and pooling. Critical for zero-allocation
  * performance.
  */
trait BufferAllocator:
  def allocateRead(): ByteBuffer
  def allocateWrite(): ByteBuffer
  def release(buffer: ByteBuffer): Unit

object BufferAllocator:
  /** Direct buffer pool - best for I/O operations. Pooled to avoid allocation
    * overhead.
    */
  def direct(size: Int, poolSize: Int = 100): BufferAllocator = new:
    private val pool = new ConcurrentLinkedQueue[ByteBuffer]()

    // Pre-allocate pool
    (0 until poolSize * 2).foreach(_ =>
      pool.add(ByteBuffer.allocateDirect(size))
    )

    def allocateRead(): ByteBuffer =
      Option(pool.poll())
        .map(_.clear())
        .getOrElse(ByteBuffer.allocateDirect(size))

    def allocateWrite(): ByteBuffer =
      Option(pool.poll())
        .map(_.clear())
        .getOrElse(ByteBuffer.allocateDirect(size))

    def release(buffer: ByteBuffer): Unit =
      if pool.size() < poolSize * 2 then
        buffer.clear()
        pool.offer(buffer)

  /** Heap buffer pool - for testing or when direct buffers aren't suitable.
    */
  def heap(size: Int, poolSize: Int = 100): BufferAllocator = new:
    private val pool = new ConcurrentLinkedQueue[ByteBuffer]()

    (0 until poolSize * 2).foreach(_ => pool.add(ByteBuffer.allocate(size)))

    def allocateRead(): ByteBuffer =
      Option(pool.poll()).map(_.clear()).getOrElse(ByteBuffer.allocate(size))

    def allocateWrite(): ByteBuffer =
      Option(pool.poll()).map(_.clear()).getOrElse(ByteBuffer.allocate(size))

    def release(buffer: ByteBuffer): Unit =
      if pool.size() < poolSize * 2 then
        buffer.clear()
        pool.offer(buffer)
