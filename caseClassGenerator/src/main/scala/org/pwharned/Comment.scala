package org.pwharned

case class Comment(
    schema: String,
    table: String,
    column: Option[String],
    comment: String
)
