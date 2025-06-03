package org.pwharned


case class Column(name: String, dataType: DataType, nullable: Option[Boolean], primary_key: Option[Boolean], generated_always_as_identity: Option[Boolean])