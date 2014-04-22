package fly.play.elasticsearch.query

import play.api.libs.json.{JsString, Json}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import fly.play.elasticsearch.JsonUtils

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-flt-query.html
 *     http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/common-options.html#fuzziness
 * Fuzziness is a string, because there is a difference between it being 1 (Int) or 1.0 (Double).
 */
case class FuzzyLikeThisQuery(
  fields: Seq[String] = FuzzyLikeThisQuery.defaultFields,
  likeText: String,
  ignoreTf: Boolean = FuzzyLikeThisQuery.defaultIgnoreTf,
  maxQueryTerms: Int = FuzzyLikeThisQuery.defaultMaxQueryTerms,
  fuzziness: String = FuzzyLikeThisQuery.defaultFuzziness,
  prefixLength: Int = FuzzyLikeThisQuery.defaultPrefixLength,
  boost: Double = FuzzyLikeThisQuery.defaultBoost,
  analyzer: String = FuzzyLikeThisQuery.defaultAnalyzer
) extends Query with JsonUtils {

  def toQueryDSL =
    Json.obj("fuzzy_like_this" ->
      toJsonObject(
        "fields" -> toJsonIfNot(fields, FuzzyLikeThisQuery.defaultFields),
        "like_text" -> Json.toJson(likeText),
        "ignore_tf" -> toJsonIfNot(ignoreTf, FuzzyLikeThisQuery.defaultIgnoreTf),
        "max_query_terms" -> toJsonIfNot(maxQueryTerms, FuzzyLikeThisQuery.defaultMaxQueryTerms),
        "fuzziness" -> toJsonIfNot(fuzziness, FuzzyLikeThisQuery.defaultFuzziness),
        "prefix_length" -> toJsonIfNot(prefixLength, FuzzyLikeThisQuery.defaultPrefixLength),
        "boost" -> toJsonIfNot(boost, FuzzyLikeThisQuery.defaultBoost),
        "analyzer" -> toJsonIfNot(analyzer, FuzzyLikeThisQuery.defaultAnalyzer)
      ))

}

object FuzzyLikeThisQuery {
  val defaultFields = Seq("_all")
  val defaultIgnoreTf = false
  val defaultMaxQueryTerms = 25
  val defaultFuzziness = "0.5"
  val defaultPrefixLength = 0
  val defaultBoost = 1.0
  val defaultAnalyzer = ""
}
