package org.pwharned.http

enum Header(val name: String):
  case ContentType extends Header("Content-Type")
  case ContentLength extends Header("Content-Length")
  case Accept extends Header("Accept")
  case Authorization extends Header("Authorization")
  case CacheControl extends Header("Cache-Control")
  case Location extends Header("Location")
  case AccessControlAllowOrigin extends Header("Access-Control-Allow-Origin")
  case AccessControlAllowMethods extends Header("Access-Control-Allow-Methods")
  case AccessControlAllowHeaders extends Header("Access-Control-Allow-Headers")
// Add more as needed...


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
    def get(key: Header): Option[String] = h.get(key.name)
    def add(key: String, value: String): Headers = h + (key -> value)
    def add(key: Header, value: String): Headers = h + (key.name -> value)
    def update(key: String, value: String): Headers = add(key, value)
    def update(key: Header, value: String): Headers = update(key.name, value)
    def asMap: Map[String, String] = h
    def merge(other: Headers): Headers = h ++ other.toMap
