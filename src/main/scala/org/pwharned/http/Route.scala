package org.pwharned.http

import org.pwharned.codec.Codec
import org.pwharned.http.HttpTypes.HttpPath
import org.pwharned.io.IO

// Type-safe route definition with opaque types
case class Route[A, B](
                        method: HttpMethod,
                        path: HttpPath,
                        handler: HttpRequest[A] => IO[HttpResponse[B]]
                      )(using val requestCodec: Codec[A], val responseCodec: Codec[B])