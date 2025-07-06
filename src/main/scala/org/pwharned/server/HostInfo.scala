package org.pwharned.server

import java.net.InetAddress
import scala.util.Try

object HostInfo:
  /**
   *  Try to fetch the local host name, falling back to "localhost".
   */
  def hostName: String =
    Try(InetAddress.getLocalHost.getHostName)
      .toOption
      .getOrElse("localhost")

  /**
   *  If you know your server port at startup, you can also build the full URL.
   */
  def baseUrl(port: Int, secure: Boolean = false): String =
    val scheme = if secure then "https" else "http"
    s"$scheme://${hostName}:$port"
