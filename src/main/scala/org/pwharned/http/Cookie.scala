package org.pwharned.http

object Cookie:
  def parse(headerValue: String): Map[String,String] =
    headerValue
      .split(";")
      .iterator
      .flatMap { pair =>
        pair.split("=", 2) match
          case Array(k, v) => Some(k.trim -> v.trim)
          case _           => None
      }
      .toMap
