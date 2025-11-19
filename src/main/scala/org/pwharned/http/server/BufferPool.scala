package org.pwharned.http.server

import org.pwharned.http.server.tcp.BufferAllocator

@deprecated("Use org.pwharned.http.server.tcp.BufferAllocator instead", "2.0")
type BufferPool = BufferAllocator

@deprecated("Use org.pwharned.http.server.tcp.BufferAllocator instead", "2.0")
object BufferPool:
  def direct(bufferSize: Int): BufferAllocator =
    BufferAllocator.direct(bufferSize)

  def heap(bufferSize: Int): BufferAllocator =
    BufferAllocator.heap(bufferSize)
