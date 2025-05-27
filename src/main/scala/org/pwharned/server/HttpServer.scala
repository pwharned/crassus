package org.pwharned.server

import io.netty.bootstrap.ServerBootstrap
import io.netty.buffer.PooledByteBufAllocator
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter, ChannelInitializer, ChannelOption, ChannelPipeline, EventLoopGroup, SimpleChannelInboundHandler}
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioServerSocketChannel
import io.netty.handler.codec.http.*

import java.nio.charset.StandardCharsets
object BasicHttpServer:

  class HttpHandler extends SimpleChannelInboundHandler[FullHttpRequest]:
    override def channelRead0(ctx: ChannelHandlerContext, request: FullHttpRequest): Unit =
      println(s"Received request: ${request.uri()}")

      val responseContent = "Hello, Netty HTTP Server!"

      // 🔥 Use Pooled Allocator for ByteBuf
      val byteBuf = ctx.alloc().buffer(responseContent.length)
      byteBuf.writeBytes(responseContent.getBytes(StandardCharsets.UTF_8))

      val response = new DefaultFullHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK, byteBuf)

      response.headers().set(HttpHeaderNames.CONTENT_TYPE, "text/plain")
      response.headers().set(HttpHeaderNames.CONTENT_LENGTH, responseContent.length)

      ctx.writeAndFlush(response)


  def startServer(): Unit =
    val bossGroup: EventLoopGroup = new NioEventLoopGroup()
    val workerGroup = new NioEventLoopGroup(Runtime.getRuntime.availableProcessors())

    try
      val bootstrap = ServerBootstrap()
      bootstrap.group(bossGroup, workerGroup)
        .channel(classOf[NioServerSocketChannel])
        .childOption(ChannelOption.ALLOCATOR, PooledByteBufAllocator.DEFAULT) // 🔥 Enable pooled buffer allocation
        .childHandler(new ChannelInitializer[SocketChannel] {
          override def initChannel(ch: SocketChannel): Unit =
            val pipeline = ch.pipeline()
            pipeline.addLast(new HttpServerCodec()) // Decodes HTTP requests
            pipeline.addLast(new HttpObjectAggregator(65536)) // Aggregates full requests
            pipeline.addLast(new HttpHandler()) // Handles responses
        })


      val channel = bootstrap.bind(8080).sync().channel()
      println("Basic HTTP Server Running on Port 8080...")
      channel.closeFuture().sync()
    finally
      bossGroup.shutdownGracefully()
      workerGroup.shutdownGracefully()

@main def runBasicServer(): Unit = BasicHttpServer.startServer()
