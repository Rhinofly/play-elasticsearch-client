package fly.play.elasticsearch.query

import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import fly.play.elasticsearch.JsonUtils

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-mlt-query.html
 */
case class MoreLikeThisQuery(
  fields: Seq[String] = MoreLikeThisQuery.defaultFields,
  likeText: String,
  percentTermsToMatch: Double = MoreLikeThisQuery.defaultPercentTermsToMatch,
  minTermFreq: Int = MoreLikeThisQuery.defaultMinTermFreq,
  maxQueryTerms: Int = MoreLikeThisQuery.defaultMaxQueryTerms,
  stopWords: Seq[String] = Seq.empty,
  minDocFreq: Int = MoreLikeThisQuery.defaultMinDocFreq,
  maxDocFreq: Int = MoreLikeThisQuery.defaultMaxDocFreq,
  minWordLength: Int = MoreLikeThisQuery.defaultMinWordLength,
  maxWordLength: Int = MoreLikeThisQuery.defaultMaxWordLength,
  boostTerms: Double = MoreLikeThisQuery.defaultBoostTerms,
  boost: Double = MoreLikeThisQuery.defaultBoost,
  analyzer: String = MoreLikeThisQuery.defaultAnalyzer
) extends Query with JsonUtils {

  def toQueryDSL =
    Json.obj("more_like_this" ->
      toJsonObject(
        "fields" -> toJsonIfNot(fields, MoreLikeThisQuery.defaultFields),
        "like_text" -> Json.toJson(likeText),
        "percent_terms_to_match" -> toJsonIfNot(percentTermsToMatch, MoreLikeThisQuery.defaultPercentTermsToMatch),
        "min_term_freq" -> toJsonIfNot(minTermFreq, MoreLikeThisQuery.defaultMinTermFreq),
        "max_query_terms" -> toJsonIfNot(maxQueryTerms, MoreLikeThisQuery.defaultMaxQueryTerms),
        "stop_words" -> toJsonIfNot(stopWords, Seq.empty),
        "min_doc_freq" -> toJsonIfNot(minDocFreq, MoreLikeThisQuery.defaultMinDocFreq),
        "max_doc_freq" -> toJsonIfNot(maxDocFreq, MoreLikeThisQuery.defaultMaxDocFreq),
        "min_word_length" -> toJsonIfNot(minWordLength, MoreLikeThisQuery.defaultMinWordLength),
        "max_word_length" -> toJsonIfNot(maxWordLength, MoreLikeThisQuery.defaultMaxWordLength),
        "boost_terms" -> toJsonIfNot(boostTerms, MoreLikeThisQuery.defaultBoostTerms),
        "boost" -> toJsonIfNot(boost, MoreLikeThisQuery.defaultBoost),
        "analyzer" -> toJsonIfNot(analyzer, MoreLikeThisQuery.defaultAnalyzer)
      ))

}

object MoreLikeThisQuery {
  val defaultFields = Seq("_all")
  val defaultPercentTermsToMatch = 0.3
  val defaultMinTermFreq = 2
  val defaultMaxQueryTerms = 25
  val defaultMinDocFreq = 5
  val defaultMaxDocFreq = 0 // this means 'unbounded'
  val defaultMinWordLength = 0
  val defaultMaxWordLength = 0 // unbounded
  val defaultBoostTerms = 1.0
  val defaultBoost = 1.0
  val defaultAnalyzer = ""
}