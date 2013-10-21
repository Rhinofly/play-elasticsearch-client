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
) extends AbstractQuery with JsonUtils {
  
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
          ).filter(isValidJsonProperty)
        )
      )
    )

}
