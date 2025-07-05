package org.pwharned.http

sealed trait Protocal[F]

sealed trait SSE[F] extends Protocal[F]
sealed trait Http[F] extends Protocal[F]
