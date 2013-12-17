package play.modules.elasticsearch.query

import play.api.libs.json._
import play.modules.elasticsearch.JsonUtils

/*
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-match-query.html
 * For the minimumShouldMatch parameter, see http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-minimum-should-match.html
 */
case class MatchQuery(
  field: String,
  value: String,
  operator: Operator.Value = Operator.or,
  matchType: MatchType.Value = MatchType.boolean,
  minimumShouldMatch: String = "",
  fuzziness: Double = -3.14,
  slop: Int = 0,
  lenient: Boolean = false
) extends Query with JsonUtils {

  def toQueryDSL =
    Json.obj( "match" ->
      Json.obj(field ->
        toJsonObject(
          "query" -> JsString(value),
          "operator" -> JsString(operator.toString()),
          "type" -> toJsonIfValid(matchType.toString, {x:String => x != MatchType.boolean.toString}),
          "minimum_should_match" -> toJsonIfValid[String](minimumShouldMatch, _ != ""),
          "fuzziness" -> toJsonIfValid[Double](fuzziness, _ >= 0.0),
          "slop" -> toJsonIfValid[Int](slop, _ > 0),
          "lenient" -> toJsonIfValid[Boolean](lenient, x => x)
        )
      )
    )

}
