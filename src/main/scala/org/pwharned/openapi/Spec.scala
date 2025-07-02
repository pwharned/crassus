package org.pwharned.openapi

import org.pwharned.json.JsonDeserializer.mapDeserializer
import org.pwharned.json.deserialize
case class root(
                      openapi: String,
                      info:      info,
                      servers:   List[server],
                      paths:     Map[String, pathItem],
                      components: components
                    )

// info
case class info(
                 version: String,
                 title:   String,
                 license: Option[license]
               )

case class license(
                    name: String
                  )

// servers
case class server(
                   url: String
                 )

// paths
case class pathItem(
                     get:  Option[operation] = None,
                     post: Option[operation] = None
                   )

case class operation(
                      summary:     String,
                      operationId: String,
                      tags:        List[String],
                      parameters:  Option[List[parameter]] = None,
                      responses:   Map[String, response]
                    )

case class parameter(
                      name:        String,
                      in:          String,
                      description: String,
                      required:    Option[Boolean]=None,
                      schema:      schema
                    )

case class response(
                     description: String,
                     headers:     Option[Map[String, header]]    = None,
                     content:     Option[Map[String, mediaType]] = None
                   )

case class header(
                   description: String,
                   schema:      schema
                 )

case class mediaType(
                      schema: schema
                    )

// components
case class components(
                       schemas: Map[String, schema]
                     )

// a fully generic schema node
case class schema(
                   `type`:     Option[String]              = None,
                   format:     Option[String]              = None,
                   items:      Option[items]              = None,
                   `$ref`:     Option[String]              = None,
                additionalProperties: Option[schema]
                 )

case class items(`type`: Option[String], `$ref`: Option[String])
@main
def test:Unit =
  val src     = scala.io.Source.fromFile("petstore.json")("UTF-8")
  val rawJson = src.mkString.stripPrefix("\uFEFF")
  src.close()
  case class nested(a: String)
  case class test(a: Map[String, nested])
  val testString  =
    """{"items":{
      |
      |                "type": "string"
      |}

      |}""".stripMargin
  rawJson.deserialize[root] match {
    case Left(value) => println(value)
    case Right(value) => println(value)
  }
