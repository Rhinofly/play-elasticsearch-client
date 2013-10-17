package play.modules.elasticsearch.query

import play.api.libs.json._

/*
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-multi-match-query.html
 */
case class MultiMatchQuery(
  fields: Seq[String],
  value: String,
  operator: Operator.Value = Operator.or,
  matchType : MatchType.Value = MatchType.boolean,
  fuzziness: Double = -3.14,
  slop: Int = 0
) extends Query {
  
  def toQueryDSL =
    Json.obj( "multi_match" -> 
      JsObject(
        Seq(
          "fields" -> Json.toJson(fields),
          "query" -> JsString(value),
          "operator" -> JsString(operator.toString()),
          "type" -> toJsonIfValid(matchType.toString, {x:String => x != MatchType.boolean.toString}),
          "fuzziness" -> toJsonIfValid(fuzziness, {x:Double => x >= 0.0}),
          "slop" -> toJsonIfValid(slop, {x:Int => x > 0})
        ).filter(isValidJsonProperty)
      )
    )

}
