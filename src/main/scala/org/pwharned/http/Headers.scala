package org.pwharned.http


object Headers:
  // Opaque type alias: headers are represented as a Map[String, String]
  opaque type Headers = Map[String, String]

  // Constructor to create a Headers instance from a Map.
  def apply(headers: Map[String, String]): Headers = headers
  

  // Provide an empty headers value.
  def empty: Headers = Map.empty

  // Extension methods to expose useful operations on Headers.
  extension (h: Headers)
    def get(key: String): Option[String] = h.get(key)
    def add(key: String, value: String): Headers = h + (key -> value)
    def update(key: String, value: String): Headers = add(key, value)
    def asMap: Map[String, String] = h