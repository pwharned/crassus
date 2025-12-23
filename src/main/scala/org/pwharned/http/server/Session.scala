package org.pwharned.http.server.tcp

import java.nio.ByteBuffer
import java.nio.channels.{SelectionKey, SocketChannel}

/** Immutable session state - pure data, no behavior. One session per TCP
  * connection.
  */
case class Session[Req](
    channel: SocketChannel,
    key: SelectionKey,
    readBuffer: ByteBuffer,
    writeBuffer: ByteBuffer,
    parser: RequestParser[Req]
):
  def isOpen: Boolean = channel.isOpen

  /** Cleanup session resources. Only behavior on Session - releasing what it
    * owns.
    */
  def close()(using alloc: BufferAllocator): Unit =
    alloc.release(readBuffer)
    alloc.release(writeBuffer)
    if channel.isOpen then channel.close()
    key.cancel()
