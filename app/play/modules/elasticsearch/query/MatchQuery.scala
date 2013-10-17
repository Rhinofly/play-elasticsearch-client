package play.modules.elasticsearch.query

import play.api.libs.json._

/*
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-match-query.html
 */
case class MatchQuery(
  field: String,
  value: String,
  operator: Operator.Value = Operator.or,
  matchType : MatchType.Value = MatchType.boolean,
  fuzziness: Double = -3.14,
  slop: Int = 0
) extends Query {
  
  def toQueryDSL =
    Json.obj( "match" -> 
      Json.obj(field ->
        JsObject(
          Seq(
            "query" -> JsString(value),
            "operator" -> JsString(operator.toString()),
            "type" -> toJsonIfValid(matchType.toString, {x:String => x != MatchType.boolean.toString}),
            "fuzziness" -> toJsonIfValid(fuzziness, {x:Double => x >= 0.0}),
            "slop" -> toJsonIfValid(slop, {x:Int => x > 0})
          ).filter({case (k, v) => (v != JsNull)})
        )
      )
    )

}

object Operator extends Enumeration {
  val or = Value("or")
  val and = Value("and")
}

object MatchType extends Enumeration {
  val boolean = Value("boolean")
  val phrase = Value("phrase")
  val phrase_prefix = Value("phrase_prefix")
}
