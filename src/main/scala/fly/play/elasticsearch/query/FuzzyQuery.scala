package fly.play.elasticsearch.query

import play.api.libs.json.{JsString, Json}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import fly.play.elasticsearch.utils.JsonUtils

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-fuzzy-query.html
 * Warning: Warning This query can be very heavy if prefix_length and max_expansions are both set to their defaults of 0. This could cause every term in the index to be examined!
 */
case class FuzzyQuery(
  field: String,
  value: String,
  fuzziness: String = FuzzyQuery.defaultFuzziness,
  prefixLength: Int = FuzzyQuery.defaultPrefixLength,
  maxExpansions: Int = FuzzyQuery.defaultMaxExpansions,
  boost: Double = FuzzyQuery.defaultBoost
) extends Query with JsonUtils {

  def toQueryDSL =
    Json.obj("fuzzy" ->
      Json.obj(field ->
        toJsonObject(
          "value" -> JsString(value),
          "fuzziness" -> toJsonIfNot(fuzziness, FuzzyQuery.defaultFuzziness),
          "prefix_length" -> toJsonIfNot(prefixLength, FuzzyQuery.defaultPrefixLength),
          "max_expansions" -> toJsonIfNot(maxExpansions, FuzzyQuery.defaultMaxExpansions),
          "boost" -> toJsonIfNot(boost, FuzzyQuery.defaultBoost)
        )
      ))

}

object FuzzyQuery {
  val defaultFuzziness = "AUTO"
  val defaultPrefixLength = 0
  val defaultMaxExpansions = 0
  val defaultBoost = 1.0
}
