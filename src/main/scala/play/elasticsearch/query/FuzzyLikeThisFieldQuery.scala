package fly.play.elasticsearch.query

import play.api.libs.json.{JsString, Json}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import fly.play.elasticsearch.utils.JsonUtils

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-flt-query.html
 *     http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/common-options.html#fuzziness
 * Fuzziness is a string, because there is a difference between it being 1 (Int) or 1.0 (Double).
 */
case class FuzzyLikeThisFieldQuery(
  field: String,
  likeText: String,
  ignoreTf: Boolean = FuzzyLikeThisFieldQuery.defaultIgnoreTf,
  maxQueryTerms: Int = FuzzyLikeThisFieldQuery.defaultMaxQueryTerms,
  fuzziness: String = FuzzyLikeThisFieldQuery.defaultFuzziness,
  prefixLength: Int = FuzzyLikeThisFieldQuery.defaultPrefixLength,
  boost: Double = FuzzyLikeThisFieldQuery.defaultBoost,
  analyzer: String = FuzzyLikeThisFieldQuery.defaultAnalyzer
) extends Query with JsonUtils {

  def toQueryDSL =
    Json.obj("fuzzy_like_this" ->
      Json.obj(field ->
        toJsonObject(
          "like_text" -> JsString(likeText),
          "ignore_tf" -> toJsonIfNot(ignoreTf, FuzzyLikeThisFieldQuery.defaultIgnoreTf),
          "max_query_terms" -> toJsonIfNot(maxQueryTerms, FuzzyLikeThisFieldQuery.defaultMaxQueryTerms),
          "fuzziness" -> toJsonIfNot(fuzziness, FuzzyLikeThisFieldQuery.defaultFuzziness),
          "prefix_length" -> toJsonIfNot(prefixLength, FuzzyLikeThisFieldQuery.defaultPrefixLength),
          "boost" -> toJsonIfNot(boost, FuzzyLikeThisFieldQuery.defaultBoost),
          "analyzer" -> toJsonIfNot(analyzer, FuzzyLikeThisFieldQuery.defaultAnalyzer)
        )
      ))

}

object FuzzyLikeThisFieldQuery {
  val defaultIgnoreTf = false
  val defaultMaxQueryTerms = 25
  val defaultFuzziness = "0.5"
  val defaultPrefixLength = 0
  val defaultBoost = 1.0
  val defaultAnalyzer = ""
}
