package fly.play.elasticsearch.query

import play.api.libs.json._
import fly.play.elasticsearch.JsonUtils

/*
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-query-string-query.html
 */
case class QueryStringQuery(
  query: String,
  defaultField: String = QueryStringQuery.defaultDefaultField,
  defaultOperator: Operator.Value = QueryStringQuery.defaultDefaultOperator,
  fields: Seq[String] = QueryStringQuery.defaultFields,
  useDisMax: Boolean = QueryStringQuery.defaultUseDisMax,
  tieBreaker: Int = QueryStringQuery.defaultTieBreaker,
  analyzer: String =QueryStringQuery.defaultAnalyzer,
  allowLeadingWildcard: Boolean = QueryStringQuery.defaultAllowLeadingWildcard,
  lowercaseExpandedTerms: Boolean = QueryStringQuery.defaultLowercaseExpandedTerms,
  enablePositionIncrements: Boolean = QueryStringQuery.defaultEnablePositionIncrements,
  fuzzyMaxExpansions: Int = QueryStringQuery.defaultFuzzyMaxExpansions,
  fuzziMinSim: Double = QueryStringQuery.defaultFuzziMinSim,
  fuzzyPrefixLength: Int = QueryStringQuery.defaultFuzzyPrefixLength,
  phraseSlop: Int = QueryStringQuery.defaultPhraseSlop,
  boost: Double = QueryStringQuery.defaultBoost,
  analyzeWildcard: Boolean = QueryStringQuery.defaultAnalyzeWildcard,
  autoGeneratePhraseQueries: Boolean = QueryStringQuery.defaultAutoGeneratePhraseQueries,
  minimumShouldMatch: String = QueryStringQuery.defaultMinimumShouldMatch,
  lenient: Boolean = QueryStringQuery.defaultLenient
) extends Query with JsonUtils {

  def toQueryDSL =
    Json.obj( "query_string" ->
      toJsonObject(
        "query" -> JsString(query),
        "default_field" -> toJsonIfNot(defaultField, QueryStringQuery.defaultDefaultField),
        "default_operator" -> toJsonIfNot(defaultOperator, QueryStringQuery.defaultDefaultOperator),
        "fields" -> toJsonIfNot(fields, QueryStringQuery.defaultFields),
        "use_dis_max" -> toJsonIfNot(useDisMax, QueryStringQuery.defaultUseDisMax),
        "tie_breaker" -> toJsonIfNot(tieBreaker, QueryStringQuery.defaultTieBreaker),
        "analyzer" -> toJsonIfNot(analyzer, QueryStringQuery.defaultAnalyzer),
        "allow_leading_wildcard" -> toJsonIfNot(allowLeadingWildcard, QueryStringQuery.defaultAllowLeadingWildcard),
        "lowercase_expanded_terms" -> toJsonIfNot(lowercaseExpandedTerms, QueryStringQuery.defaultLowercaseExpandedTerms),
        "enable_position_increments" -> toJsonIfNot(enablePositionIncrements, QueryStringQuery.defaultEnablePositionIncrements),
        "fuzzy_max_expansions" -> toJsonIfNot(fuzzyMaxExpansions, QueryStringQuery.defaultFuzzyMaxExpansions),
        "fuzzy_min_sim" -> toJsonIfNot(fuzziMinSim, QueryStringQuery.defaultFuzziMinSim),
        "fuzzy_prefix_length" -> toJsonIfNot(fuzzyPrefixLength, QueryStringQuery.defaultFuzzyPrefixLength),
        "phrase_slop" -> toJsonIfNot(phraseSlop, QueryStringQuery.defaultPhraseSlop),
        "boost" -> toJsonIfNot(boost, QueryStringQuery.defaultBoost),
        "analyze_wildcard" -> toJsonIfNot(analyzeWildcard, QueryStringQuery.defaultAnalyzeWildcard),
        "auto_generate_phrase_queries" -> toJsonIfNot(autoGeneratePhraseQueries, QueryStringQuery.defaultAutoGeneratePhraseQueries),
        "minimum_should_match" -> toJsonIfNot(minimumShouldMatch, QueryStringQuery.defaultMinimumShouldMatch),
        "lenient" -> toJsonIfNot(lenient, QueryStringQuery.defaultLenient)
      )
    )

}

object QueryStringQuery {
  val defaultDefaultField: String = ""
  val defaultDefaultOperator: Operator.Value = Operator.or
  val defaultFields: Seq[String] = Seq.empty
  val defaultUseDisMax: Boolean = true
  val defaultTieBreaker: Int = 0
  val defaultAnalyzer: String = ""
  val defaultAllowLeadingWildcard: Boolean = true
  val defaultLowercaseExpandedTerms: Boolean = true
  val defaultEnablePositionIncrements: Boolean = true
  val defaultFuzzyMaxExpansions: Int = 50
  val defaultFuzziMinSim: Double = 0.5
  val defaultFuzzyPrefixLength: Int = 0
  val defaultPhraseSlop: Int = 0
  val defaultBoost: Double = 1.0
  val defaultAnalyzeWildcard: Boolean = false
  val defaultAutoGeneratePhraseQueries: Boolean = false
  val defaultMinimumShouldMatch: String = ""
  val defaultLenient: Boolean = false
}
