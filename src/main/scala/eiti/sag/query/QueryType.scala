package eiti.sag.query

object QueryType extends Enumeration {
  val Color,
  Feeding,
  Size,
  Weight,
  Classification, //mammal, reptile, etc.
  Location,
  General,
  None = Value
}
