package eiti.sag.query

object QueryMap {
  val keywordListToQueryTypeMap = Map(
    List("where") -> QueryType.Location,
    List("color", "colour") -> QueryType.Color,
    List("food", "eat", "eats") -> QueryType.Feeding,
    List("species") -> QueryType.Classification,
    List("weight") -> QueryType.Weight,
    List("general","info") -> QueryType.General)
}
