package eiti.sag.query

object KnownPosTags extends Enumeration {
  type KnownPosTags = KnownPosTagValue

  val PRP = KnownPosTagValue(nextId, "PRP") // zaimek osobowy
  val JJ = KnownPosTagValue(nextId, "JJ") // przymiotnik (Adjective)
  val JJR = KnownPosTagValue(nextId, "JJR") // przymiotnik porównywalny (comparative)
  val JJS = KnownPosTagValue(nextId, "JJS") // przymiotnik najwyższy (superlative)

  case class KnownPosTagValue(key: Int, asString: String)

  def fromString(str: String): KnownPosTags = {
    if(str.equals("PRP")) PRP
    else if(str.equals("JJ")) JJ
    else if(str.equals("JJR")) JJR
    else if(str.equals("JJS")) JJS
    else null
  }
}