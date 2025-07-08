package org.pwharned


case class Column(name: String, dataType: SqlDataType, nullable: Option[Boolean], primary_key: Option[Boolean], generated_always_as_identity: Option[Boolean], default:Option[Boolean])
case class PrimaryKey(schema: String, table: String, columns: Seq[String])