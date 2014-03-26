package play.modules.elasticsearch.query

import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.modules.elasticsearch.JsonUtils

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-mlt-field-query.html
 */
case class MoreLikeThisFieldQuery(
  field: String,
  likeText: String,
  percentTermsToMatch: Double = MoreLikeThisFieldQuery.defaultPercentTermsToMatch,
  minTermFreq: Int = MoreLikeThisFieldQuery.defaultMinTermFreq,
  maxQueryTerms: Int = MoreLikeThisFieldQuery.defaultMaxQueryTerms,
  stopWords: Seq[String] = Seq.empty,
  minDocFreq: Int = MoreLikeThisFieldQuery.defaultMinDocFreq,
  maxDocFreq: Int = MoreLikeThisFieldQuery.defaultMaxDocFreq,
  minWordLength: Int = MoreLikeThisFieldQuery.defaultMinWordLength,
  maxWordLength: Int = MoreLikeThisFieldQuery.defaultMaxWordLength,
  boostTerms: Double = MoreLikeThisFieldQuery.defaultBoostTerms,
  boost: Double = MoreLikeThisFieldQuery.defaultBoost,
  analyzer: String = MoreLikeThisFieldQuery.defaultAnalyzer
) extends Query with JsonUtils {

  def toQueryDSL =
    Json.obj("more_like_this" -> Json.obj(field ->
      toJsonObject(
        "like_text" -> Json.toJson(likeText),
        "percent_terms_to_match" -> toJsonIfNot(percentTermsToMatch, MoreLikeThisFieldQuery.defaultPercentTermsToMatch),
        "min_term_freq" -> toJsonIfNot(minTermFreq, MoreLikeThisFieldQuery.defaultMinTermFreq),
        "max_query_terms" -> toJsonIfNot(maxQueryTerms, MoreLikeThisFieldQuery.defaultMaxQueryTerms),
        "stop_words" -> toJsonIfNot(stopWords, Seq.empty),
        "min_doc_freq" -> toJsonIfNot(minDocFreq, MoreLikeThisFieldQuery.defaultMinDocFreq),
        "max_doc_freq" -> toJsonIfNot(maxDocFreq, MoreLikeThisFieldQuery.defaultMaxDocFreq),
        "min_word_length" -> toJsonIfNot(minWordLength, MoreLikeThisFieldQuery.defaultMinWordLength),
        "max_word_length" -> toJsonIfNot(maxWordLength, MoreLikeThisFieldQuery.defaultMaxWordLength),
        "boost_terms" -> toJsonIfNot(boostTerms, MoreLikeThisFieldQuery.defaultBoostTerms),
        "boost" -> toJsonIfNot(boost, MoreLikeThisFieldQuery.defaultBoost),
        "analyzer" -> toJsonIfNot(analyzer, MoreLikeThisFieldQuery.defaultAnalyzer)
      )))

}

object MoreLikeThisFieldQuery {
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