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
  matchType: MatchType.Value = MatchQuery.defaultMatchType,
  operator: Operator.Value = MatchQuery.defaultOperator,
  minimumShouldMatch: String = MatchQuery.defaultMinimumShouldMatch,
  analyzer: String =MatchQuery.defaultAnalyzer,
  fuzziness: Double = MatchQuery.defaultFuzziness,
  fuzzyPrefixLength: Int = MatchQuery.defaultFuzzyPrefixLength,
  fuzzyMaxExpansions: Int = MatchQuery.defaultFuzzyMaxExpansions,
  cutoffFrequency: Double = MatchQuery.defaultCutoffFrequency,
  slop: Int = MatchQuery.defaultSlop,
  lenient: Boolean = MatchQuery.defaultLenient
) extends Query with JsonUtils {

  def toQueryDSL =
    Json.obj( "match" ->
      Json.obj(field ->
        toJsonObject(
          "query" -> JsString(value),
          "type" -> toJsonIfNot(matchType, MatchQuery.defaultMatchType),
          "operator" -> toJsonIfNot(operator, MatchQuery.defaultOperator),
          "minimum_should_match" -> toJsonIfNot(minimumShouldMatch, MatchQuery.defaultMinimumShouldMatch),
          "analyzer" -> toJsonIfNot(analyzer, MatchQuery.defaultAnalyzer),
          "fuzziness" -> toJsonIfNot(fuzziness, MatchQuery.defaultFuzziness),
          "prefix_length" -> toJsonIfNot(fuzzyPrefixLength, MatchQuery.defaultFuzzyPrefixLength),
          "max_expansions" -> toJsonIfNot(fuzzyMaxExpansions, MatchQuery.defaultFuzzyMaxExpansions),
          "cutoff_frequency" -> toJsonIfNot(cutoffFrequency, MatchQuery.defaultCutoffFrequency),
          "slop" -> toJsonIfNot(slop, MatchQuery.defaultSlop),
          "lenient" -> toJsonIfNot(lenient, MatchQuery.defaultLenient)
        )
      )
    )

}

object MatchQuery {
  val defaultMatchType: MatchType.Value = MatchType.boolean
  val defaultOperator: Operator.Value = Operator.or
  val defaultMinimumShouldMatch: String = ""
  val defaultAnalyzer: String = ""
  val defaultFuzziness: Double = -1.0
  val defaultFuzzyPrefixLength: Int = 0
  val defaultFuzzyMaxExpansions: Int = 50
  val defaultCutoffFrequency = -1.0
  val defaultSlop: Int = 0
  val defaultLenient: Boolean = false
}
