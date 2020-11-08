package eiti.sag.query

import eiti.sag.TranslationAgent.TaggedQuery

case class UsersQueryInstance(originalQuery: String, parsedType: QueryType.Value, tagged: TaggedQuery, mainWords :Array[String], animal :String) {

}
