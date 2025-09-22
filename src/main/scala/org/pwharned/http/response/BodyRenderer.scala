package org.pwharned.http.response


import java.nio.ByteBuffer
import java.nio.channels.SocketChannel
import java.nio.charset.StandardCharsets

trait BodyRenderer[B] {
  def renderBody(
                  body:     B,
                  writeBuf: ByteBuffer,
                  channel:  SocketChannel
                ): Unit
}


object BodyRenderer {
  // Strict ByteBuffer
  ///implicit val fullBuffer: BodyRenderer[ByteBuffer] = ???
  // Chunked (()=>Option[ByteBuffer])
  // Add SSE, FileStream, etc.
  given byteBufferBodyRenderer: BodyRenderer[ByteBuffer] =
  new BodyRenderer[ByteBuffer] {
    override def renderBody(bodyContainer: ByteBuffer, writeBuf: ByteBuffer, channel: SocketChannel): Unit = {
      // Ensure the bodyContainer is ready for reading
      //bodyContainer.flip() // This might already be done by the serializer, but good to be safe.
      while (bodyContainer.hasRemaining) {
        channel.write(bodyContainer)
      }
    }
  }

  // A BodyRenderer for String, in case an EntitySerializer produces a String directly.
  given stringBodyRenderer: BodyRenderer[String] =
  new BodyRenderer[String] {
    override def renderBody(bodyContainer: String, writeBuf: ByteBuffer, channel: SocketChannel): Unit = {
      // Convert the string to bytes and write
      val bytes = bodyContainer.getBytes(StandardCharsets.UTF_8)
      writeBuf.clear()
      writeBuf.put(bytes)
      writeBuf.flip()
      while (writeBuf.hasRemaining) {
        channel.write(writeBuf)
      }
    }
  }

  given chunkedBodyRenderer: BodyRenderer[() => Option[ByteBuffer]] =
    new BodyRenderer[() => Option[ByteBuffer]] {
      private val CRLF = "\r\n".getBytes(StandardCharsets.US_ASCII)

      def renderBody(
                      nextChunk: () => Option[ByteBuffer],
                      writeBuf: java.nio.ByteBuffer,
                      channel: java.nio.channels.SocketChannel
                    ): Unit = {
        // 1) flush headers
        while (writeBuf.hasRemaining) channel.write(writeBuf)

        // 2) stream each chunk
        @annotation.tailrec
        def loop(): Unit = nextChunk() match {
          case Some(buf) =>
            // chunk‐size line
            val sizeLine = f"${buf.remaining}%x".getBytes(StandardCharsets.US_ASCII)
            channel.write(java.nio.ByteBuffer.wrap(sizeLine));
            channel.write(java.nio.ByteBuffer.wrap(CRLF))
            // chunk data + CRLF
            channel.write(buf);
            channel.write(java.nio.ByteBuffer.wrap(CRLF))
            loop()

          case None =>
            // final zero‐length chunk
            channel.write(java.nio.ByteBuffer.wrap("0\r\n\r\n".getBytes(StandardCharsets.US_ASCII)))
        }

        loop()
      }
    }
}