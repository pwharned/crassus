package org.pwharned

sealed trait DataType {
  def sqlName: String
}
case object Int extends DataType{
  val sqlName =  "INTEGER"
}
case object String extends DataType {
  val sqlName =  "TEXT"
}
case object  Boolean extends DataType{
  val sqlName = "BOOLEAN"
}
case object Float extends DataType{
  val sqlName = "FLOAT"
}
case object Date extends DataType{
  val sqlName = "DATE"
}



object DataType {
  val values: List[DataType] = List(Int, String, Boolean, Float)
  def fromString(s: String): Option[DataType] = values.find(_.sqlName.equalsIgnoreCase(s))
}