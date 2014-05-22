package fly.play.elasticsearch.analysis

import play.api.libs.functional.syntax._
import play.api.libs.json.{Format, JsError, JsObject, Json, JsResult, JsString, JsSuccess, JsUndefined, JsValue, Reads, Writes, __}
import fly.play.elasticsearch.utils.JsonUtils

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-tokenfilters.html
 * ElasticSearch uses the terms "filter" and "token filter" interchangeably.
 */
sealed trait TokenFilter {
  def name: String
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-standard-tokenfilter.html
 */
case class StandardTokenFilter(name: String) extends TokenFilter
object StandardTokenFilter extends JsonUtils {
  implicit val format: Format[StandardTokenFilter] = (__ \ "name").format[String].inmap(StandardTokenFilter(_), _.name)
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-asciifolding-tokenfilter.html
 */
case class AsciiFoldingTokenFilter(name: String) extends TokenFilter
object AsciiFoldingTokenFilter extends JsonUtils {
  implicit val format: Format[AsciiFoldingTokenFilter] = (__ \ "name").format[String].inmap(AsciiFoldingTokenFilter(_), _.name)
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-length-tokenfilter.html
 */
case class LengthTokenFilter(
    name: String,
    min: Option[Int] = None,
    max: Option[Int] = None
  ) extends TokenFilter

object LengthTokenFilter extends JsonUtils {
  implicit val format: Format[LengthTokenFilter] = (
    (__ \ "name").format[String] and
    (__ \ "min").formatNullable[Int] and
    (__ \ "max").formatNullable[Int]
    )(LengthTokenFilter.apply, unlift(LengthTokenFilter.unapply))
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-lowercase-tokenfilter.html
 * Lowercase token filter supports Greek and Turkish lowercase token filters through the language parameter.
 */
case class LowercaseTokenFilter(
    name: String,
    language: Option[String] = None
) extends TokenFilter

object LowercaseTokenFilter {
  implicit val format: Format[LowercaseTokenFilter] = (
    (__ \ "name").format[String] and
    (__ \ "language").formatNullable[String]
    )(LowercaseTokenFilter.apply, unlift(LowercaseTokenFilter.unapply))
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-ngram-tokenfilter.html
 */
case class NGramTokenFilter(
    name: String,
    minGram: Int = NGramTokenFilter.defaultMinGram,
    maxGram: Int = NGramTokenFilter.defaultMaxGram
) extends TokenFilter

object NGramTokenFilter extends JsonUtils {
  val defaultMinGram = 1
  val defaultMaxGram = 2
  implicit val format: Format[NGramTokenFilter] = (
    (__ \ "name").format[String] and
    formatWithDefault(__ \ "min_gram", defaultMinGram) and
    formatWithDefault(__ \ "max_gram", defaultMaxGram)
    )(NGramTokenFilter.apply, unlift(NGramTokenFilter.unapply))
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-edgengram-tokenfilter.html
 */
case class EdgeNGramTokenFilter(
    name: String,
    minGram: Int = EdgeNGramTokenFilter.defaultMinGram,
    maxGram: Int = EdgeNGramTokenFilter.defaultMaxGram,
    side: String = EdgeNGramTokenFilter.defaultSide
) extends TokenFilter

object EdgeNGramTokenFilter extends JsonUtils {
  val defaultMinGram = 1
  val defaultMaxGram = 2
  val defaultSide = "front"
  implicit val format: Format[EdgeNGramTokenFilter] = (
    (__ \ "name").format[String] and
    formatWithDefault(__ \ "min_gram", defaultMinGram) and
    formatWithDefault(__ \ "max_gram", defaultMaxGram) and
    formatWithDefault(__ \ "side", defaultSide)
    )(EdgeNGramTokenFilter.apply, unlift(EdgeNGramTokenFilter.unapply))
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-porterstem-tokenfilter.html
 */
case class PorterStemTokenFilter(name: String) extends TokenFilter
object PorterStemTokenFilter extends JsonUtils {
  implicit val format: Format[PorterStemTokenFilter] = (__ \ "name").format[String].inmap(PorterStemTokenFilter(_), _.name)
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-shingle-tokenfilter.html
 */
case class ShingleTokenFilter(
    name: String,
    maxShingleSize: Int = ShingleTokenFilter.defaultMaxShingleSize,
    minShingleSize: Int = ShingleTokenFilter.defaultMinShingleSize,
    outputUnigrams: Boolean = ShingleTokenFilter.defaultOutputUnigrams,
    outputUnigramsIfNoShingles: Boolean = ShingleTokenFilter.defaultOutputUnigramsIfNoShingles,
    tokenSeparator: Option[String] = None
) extends TokenFilter

object ShingleTokenFilter extends JsonUtils {
  val defaultMaxShingleSize = 2
  val defaultMinShingleSize = 2
  val defaultOutputUnigrams = true
  val defaultOutputUnigramsIfNoShingles = false
  implicit val format: Format[ShingleTokenFilter] = (
    (__ \ "name").format[String] and
    formatWithDefault(__ \ "max_shingle_size", defaultMaxShingleSize) and
    formatWithDefault(__ \ "min_shingle_size", defaultMinShingleSize) and
    formatWithDefault(__ \ "output_unigrams", defaultOutputUnigrams) and
    formatWithDefault(__ \ "output_unigrams_if_no_shingles", defaultOutputUnigramsIfNoShingles) and
    (__ \ "token_separator").formatNullable[String]
    )(ShingleTokenFilter.apply, unlift(ShingleTokenFilter.unapply))
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-stop-tokenfilter.html
 */
case class StopTokenFilter(
    name: String,
    stopwords: Option[Seq[String]] = None,
    stopwordsPath: Option[String] = None,
    enablePositionIncrements: Boolean = StopTokenFilter.defaultEnablePositionIncrements,
    ignoreCase: Boolean = StopTokenFilter.defaultIgnoreCase,
    removeTrailing: Boolean = StopTokenFilter.defaultRemoveTrailing
) extends TokenFilter

object StopTokenFilter extends JsonUtils {
  val defaultEnablePositionIncrements = true
  val defaultIgnoreCase = false
  val defaultRemoveTrailing = true
  implicit val format: Format[StopTokenFilter] = (
    (__ \ "name").format[String] and
    (__ \ "stopwords").formatNullable[Seq[String]] and
    (__ \ "stopwords_path").formatNullable[String] and
    formatWithDefault(__ \ "enable_position_increments", defaultEnablePositionIncrements) and
    formatWithDefault(__ \ "ignore_case", defaultIgnoreCase) and
    formatWithDefault(__ \ "remove_trailing", defaultRemoveTrailing)
    )(StopTokenFilter.apply, unlift(StopTokenFilter.unapply))
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-word-delimiter-tokenfilter.html
 */
case class WordDelimiterTokenFilter(
    name: String,
    generateWordParts: Boolean = WordDelimiterTokenFilter.defaultGenerateWordParts,
    generateNumberParts: Boolean = WordDelimiterTokenFilter.defaultGenerateNumberParts,
    catenateWords: Boolean = WordDelimiterTokenFilter.defaultCatenateWords,
    catenateNumbers: Boolean = WordDelimiterTokenFilter.defaultCatenateNumbers,
    catenateAll: Boolean = WordDelimiterTokenFilter.defaultCatenateAll,
    splitOnCaseChange: Boolean = WordDelimiterTokenFilter.defaultSplitOnCaseChange,
    preserveOriginal: Boolean = WordDelimiterTokenFilter.defaultPreserveOriginal,
    splitOnNumerics: Boolean = WordDelimiterTokenFilter.defaultSplitOnNumerics,
    stemEnglishPossessive: Boolean = WordDelimiterTokenFilter.defaultStemEnglishPossessive,
    protectedWords: Option[Seq[String]] = None,
    protectedWordsPath: Option[String] = None,
    typeTablePath: Option[String] = None
) extends TokenFilter

object WordDelimiterTokenFilter extends JsonUtils {
  val defaultGenerateWordParts = true
  val defaultGenerateNumberParts = true
  val defaultCatenateWords = false
  val defaultCatenateNumbers = false
  val defaultCatenateAll = false
  val defaultSplitOnCaseChange = true
  val defaultPreserveOriginal = false
  val defaultSplitOnNumerics = true
  val defaultStemEnglishPossessive = true
  implicit val format: Format[WordDelimiterTokenFilter] = (
    (__ \ "name").format[String] and
    formatWithDefault(__ \ "generate_word_parts", defaultGenerateWordParts) and
    formatWithDefault(__ \ "generate_number_parts", defaultGenerateNumberParts) and
    formatWithDefault(__ \ "catenate_words", defaultCatenateWords) and
    formatWithDefault(__ \ "catenate_numbers", defaultCatenateNumbers) and
    formatWithDefault(__ \ "catenate_all", defaultCatenateAll) and
    formatWithDefault(__ \ "split_on_case_change", defaultSplitOnCaseChange) and
    formatWithDefault(__ \ "preserve_original", defaultPreserveOriginal) and
    formatWithDefault(__ \ "split_on_numerics", defaultSplitOnNumerics) and
    formatWithDefault(__ \ "stem_english_possessive", defaultStemEnglishPossessive) and
    (__ \ "protected_words").formatNullable[Seq[String]] and
    (__ \ "protected_words_path").formatNullable[String] and
    (__ \ "type_table_path").formatNullable[String]
    )(WordDelimiterTokenFilter.apply, unlift(WordDelimiterTokenFilter.unapply))
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-stemmer-tokenfilter.html
 */

case class StemmerTokenFilter(
    name: String,
    language: String
) extends TokenFilter

object StemmerTokenFilter {
  implicit val format: Format[StemmerTokenFilter] = (
    (__ \ "name").format[String] and
    (__ \ "language").format[String]
    )(StemmerTokenFilter.apply, unlift(StemmerTokenFilter.unapply))

  /**
   * Normalize a stemmer definition with a "name" field, because we use the name field ourselves. This will transform
   * { ...,
   *   "my_stemmer" : {
   *     "type" : "stemmer",
   *     "name" : "light_german"
   *   }, ...
   * }
   * into
   * { ...,
   *   "my_stemmer" : {
   *     "type" : "stemmer",
   *     "language" : "light_german"
   *   }, ...
   * }
   */
  def normalizeJson(json: JsValue) =
    if ((json \\ "name").isEmpty)
      json
    else
      new JsObject(json.as[JsObject].fields map { case(componentName, componentDefinition) =>
        val componentDefinitionNorm = new JsObject(componentDefinition.as[JsObject].fields map {case(key, value) =>
          if (key == "name") ("language", value) else (key, value)
        })
        (componentName, componentDefinitionNorm)
      })
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-stemmer-override-tokenfilter.html
 */

case class StemmerOverrideTokenFilter(
    name: String,
    rulesPath: Option[String] = None
) extends TokenFilter

object StemmerOverrideTokenFilter {
  implicit val format: Format[StemmerOverrideTokenFilter] = (
    (__ \ "name").format[String] and
    (__ \ "rules_path").formatNullable[String]
    )(StemmerOverrideTokenFilter.apply, unlift(StemmerOverrideTokenFilter.unapply))
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-keyword-marker-tokenfilter.html
 */

case class KeywordMarkerTokenFilter(
    name: String,
    keywords: Option[Seq[String]] = None,
    keywordsPath: Option[String] = None,
    ignoreCase: Boolean = KeywordMarkerTokenFilter.defaultIgnoreCase
) extends TokenFilter

object KeywordMarkerTokenFilter extends JsonUtils {
  val defaultIgnoreCase = false
  implicit val format: Format[KeywordMarkerTokenFilter] = (
    (__ \ "name").format[String] and
    (__ \ "keywords").formatNullable[Seq[String]] and
    (__ \ "keywords_path").formatNullable[String] and
    formatWithDefault(__ \ "ignore_case", defaultIgnoreCase)
    )(KeywordMarkerTokenFilter.apply, unlift(KeywordMarkerTokenFilter.unapply))
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-keyword-repeat-tokenfilter.html
 */
case class KeywordRepeatTokenFilter(name: String) extends TokenFilter
object KeywordRepeatTokenFilter extends JsonUtils {
  implicit val format: Format[KeywordRepeatTokenFilter] = (__ \ "name").format[String].inmap(KeywordRepeatTokenFilter(_), _.name)
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-kstem-tokenfilter.html
 */
case class KStemTokenFilter(name: String) extends TokenFilter
object KStemTokenFilter extends JsonUtils {
  implicit val format: Format[KStemTokenFilter] = (__ \ "name").format[String].inmap(KStemTokenFilter(_), _.name)
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-snowball-tokenfilter.html
 */
case class SnowballTokenFilter(
    name: String,
    language: String
) extends TokenFilter
object SnowballTokenFilter {
  implicit val format: Format[SnowballTokenFilter] = (
    (__ \ "name").format[String] and
    (__ \ "language").format[String]
    )(SnowballTokenFilter.apply, unlift(SnowballTokenFilter.unapply))
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-phonetic-tokenfilter.html
 * and https://github.com/elasticsearch/elasticsearch-analysis-phonetic
 */
case class PhoneticTokenFilter(
    name: String,
    encoder: String,
    replace: Boolean = PhoneticTokenFilter.defaultReplace
) extends TokenFilter
object PhoneticTokenFilter extends JsonUtils {
  val defaultReplace = true
  implicit val format: Format[PhoneticTokenFilter] = (
    (__ \ "name").format[String] and
    (__ \ "encoder").format[String] and
    formatWithDefault((__ \ "replace"), defaultReplace)
    )(PhoneticTokenFilter.apply, unlift(PhoneticTokenFilter.unapply))
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-synonym-tokenfilter.html
 */
case class SynonymTokenFilter(
    name: String,
    format: Option[String] = None,
    synonyms: Option[Seq[String]] = None,
    synonymsPath: Option[String] = None,
    ignoreCase: Boolean = SynonymTokenFilter.defaultIgnoreCase,
    expand : Boolean = SynonymTokenFilter.defaultExpand,
    tokenizer: Option[String] = None
) extends TokenFilter
object SynonymTokenFilter extends JsonUtils {
  val defaultIgnoreCase = false
  val defaultExpand = true
  implicit val format: Format[SynonymTokenFilter] = (
    (__ \ "name").format[String] and
    (__ \ "format").formatNullable[String] and
    (__ \ "synonyms").formatNullable[Seq[String]] and
    (__ \ "synonyms_path").formatNullable[String] and
    formatWithDefault((__ \ "ignore_case"), defaultIgnoreCase) and
    formatWithDefault((__ \ "expand"), defaultExpand) and
    (__ \ "tokenizer").formatNullable[String]
    )(SynonymTokenFilter.apply, unlift(SynonymTokenFilter.unapply))
}

/**
 *  See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-compound-word-tokenfilter.html
 */
case class DictionaryDecompounderTokenFilter(
    name: String,
    wordList: Option[Seq[String]] = None,
    wordListPath: Option[String] = None,
    minWordSize: Int = DictionaryDecompounderTokenFilter.defaultMinWordSize,
    minSubwordSize: Int = DictionaryDecompounderTokenFilter.defaultMinSubwordSize,
    maxSubwordSize: Int = DictionaryDecompounderTokenFilter.defaultMaxSubwordSize,
    onlyLongestMatch: Boolean = DictionaryDecompounderTokenFilter.defaultOnlyLongestMatch
) extends TokenFilter
object DictionaryDecompounderTokenFilter extends JsonUtils {
  val defaultMinWordSize = 5
  val defaultMinSubwordSize = 2
  val defaultMaxSubwordSize = 15
  val defaultOnlyLongestMatch = false
  implicit val format: Format[DictionaryDecompounderTokenFilter] = (
    (__ \ "name").format[String] and
    (__ \ "word_list").formatNullable[Seq[String]] and
    (__ \ "word_list_path").formatNullable[String] and
    formatWithDefault((__ \ "min_word_size"), defaultMinWordSize) and
    formatWithDefault((__ \ "min_subword_size"), defaultMinSubwordSize) and
    formatWithDefault((__ \ "max_subword_size"), defaultMaxSubwordSize) and
    formatWithDefault((__ \ "only_longest_match"), defaultOnlyLongestMatch)
    )(DictionaryDecompounderTokenFilter.apply, unlift(DictionaryDecompounderTokenFilter.unapply))
}

/**
 *  See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-compound-word-tokenfilter.html
 */
case class HyphenationDecompounderTokenFilter(
    name: String,
    wordList: Option[Seq[String]] = None,
    wordListPath: Option[String] = None,
    minWordSize: Int = HyphenationDecompounderTokenFilter.defaultMinWordSize,
    minSubwordSize: Int = HyphenationDecompounderTokenFilter.defaultMinSubwordSize,
    maxSubwordSize: Int = HyphenationDecompounderTokenFilter.defaultMaxSubwordSize,
    onlyLongestMatch: Boolean = HyphenationDecompounderTokenFilter.defaultOnlyLongestMatch
) extends TokenFilter
object HyphenationDecompounderTokenFilter extends JsonUtils {
  val defaultMinWordSize = 5
  val defaultMinSubwordSize = 2
  val defaultMaxSubwordSize = 15
  val defaultOnlyLongestMatch = false
  implicit val format: Format[HyphenationDecompounderTokenFilter] = (
    (__ \ "name").format[String] and
    (__ \ "word_list").formatNullable[Seq[String]] and
    (__ \ "word_list_path").formatNullable[String] and
    formatWithDefault((__ \ "min_word_size"), defaultMinWordSize) and
    formatWithDefault((__ \ "min_subword_size"), defaultMinSubwordSize) and
    formatWithDefault((__ \ "max_subword_size"), defaultMaxSubwordSize) and
    formatWithDefault((__ \ "only_longest_match"), defaultOnlyLongestMatch)
    )(HyphenationDecompounderTokenFilter.apply, unlift(HyphenationDecompounderTokenFilter.unapply))
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-reverse-tokenfilter.html
 */
case class ReverseTokenFilter(name: String) extends TokenFilter
object ReverseTokenFilter extends JsonUtils {
  implicit val format: Format[ReverseTokenFilter] = (__ \ "name").format[String].inmap(ReverseTokenFilter(_), _.name)
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-elision-tokenfilter.html
 */
case class ElisionTokenFilter(
    name: String,
    articles: Seq[String]
) extends TokenFilter
object ElisionTokenFilter {
  implicit val format: Format[ElisionTokenFilter] = (
    (__ \ "name").format[String] and
    (__ \ "articles").format[Seq[String]]
  )(ElisionTokenFilter.apply, unlift(ElisionTokenFilter.unapply))
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-truncate-tokenfilter.html
 */
case class TruncateTokenFilter(
    name: String,
    length: Int = TruncateTokenFilter.defaultLength
) extends TokenFilter
object TruncateTokenFilter extends JsonUtils {
  val defaultLength = 10
  implicit val format: Format[TruncateTokenFilter] = (
    (__ \ "name").format[String] and
    formatWithDefault(__ \ "length", defaultLength)
  )(TruncateTokenFilter.apply, unlift(TruncateTokenFilter.unapply))
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-unique-tokenfilter.html
 */
case class UniqueTokenFilter(
    name: String,
    onlyOnSamePosition: Boolean = UniqueTokenFilter.defaultOnlyOnSamePosition
) extends TokenFilter
object UniqueTokenFilter extends JsonUtils {
  val defaultOnlyOnSamePosition = false
  implicit val format: Format[UniqueTokenFilter] = (
    (__ \ "name").format[String] and
    formatWithDefault(__ \ "only_on_same_position", defaultOnlyOnSamePosition)
  )(UniqueTokenFilter.apply, unlift(UniqueTokenFilter.unapply))
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-pattern-capture-tokenfilter.html
 */
case class PatternCaptureTokenFilter(
    name: String,
    preserveOriginal: Boolean = PatternCaptureTokenFilter.defaultPreserveOriginal,
    patterns: Seq[String]
) extends TokenFilter
object PatternCaptureTokenFilter extends JsonUtils {
  val defaultPreserveOriginal = true
  implicit val format: Format[PatternCaptureTokenFilter] = (
    (__ \ "name").format[String] and
    formatWithDefault(__ \ "preserve_original", defaultPreserveOriginal) and
    (__ \ "patterns").format[Seq[String]]
  )(PatternCaptureTokenFilter.apply, unlift(PatternCaptureTokenFilter.unapply))
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-pattern_replace-tokenfilter.html
 */
case class PatternReplaceTokenFilter(
    name: String,
    pattern: String,
    replacement: String
) extends TokenFilter
object PatternReplaceTokenFilter {
  implicit val format: Format[PatternReplaceTokenFilter] = (
    (__ \ "name").format[String] and
    (__ \ "pattern").format[String] and
    (__ \ "replacement").format[String]
  )(PatternReplaceTokenFilter.apply, unlift(PatternReplaceTokenFilter.unapply))
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-trim-tokenfilter.html
 */
case class TrimTokenFilter(
    name: String
) extends TokenFilter
object TrimTokenFilter {
  implicit val format: Format[TrimTokenFilter] = (__ \ "name").format[String].inmap(TrimTokenFilter(_), _.name)
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-limit-token-count-tokenfilter.html
 */
case class LimitTokenCountTokenFilter(
    name: String,
    maxTokenCount: Int = LimitTokenCountTokenFilter.defaultMaxTokenCount,
    consumeAllTokens: Boolean = LimitTokenCountTokenFilter.defaultConsumeAllTokens
) extends TokenFilter
object LimitTokenCountTokenFilter extends JsonUtils {
  val defaultMaxTokenCount = 1
  val defaultConsumeAllTokens = false
  implicit val format: Format[LimitTokenCountTokenFilter] = (
    (__ \ "name").format[String] and
    formatWithDefault(__ \ "max_token_count", defaultMaxTokenCount) and
    formatWithDefault(__ \ "consume_all_tokens", defaultConsumeAllTokens)
  )(LimitTokenCountTokenFilter.apply, unlift(LimitTokenCountTokenFilter.unapply))
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-hunspell-tokenfilter.html
 * The settings are `Option`s, so they do not interfere with global settings as described in the documentation.
 */
case class HunspellTokenFilter(
    name: String,
    ignoreCase: Option[Boolean] = None,
    strictAffixParsing: Option[Boolean] = None
) extends TokenFilter
object HunspellTokenFilter {
  implicit val format: Format[HunspellTokenFilter] = (
    (__ \ "name").format[String] and
    (__ \ "ignore_case").formatNullable[Boolean] and
    (__ \ "strict_affix_parsing").formatNullable[Boolean]
  )(HunspellTokenFilter.apply, unlift(HunspellTokenFilter.unapply))
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-common-grams-tokenfilter.html
 */
case class CommonGramsTokenFilter(
    name: String,
    commonWords: Option[Seq[String]] = None,
    commonWordsPath: Option[String] = None,
    ignoreCase: Boolean = CommonGramsTokenFilter.defaultIgnoreCase,
    queryMode: Boolean = CommonGramsTokenFilter.defaultQueryMode
) extends TokenFilter
object CommonGramsTokenFilter extends JsonUtils {
  val defaultIgnoreCase = false
  val defaultQueryMode = false
  implicit val format: Format[CommonGramsTokenFilter] = (
    (__ \ "name").format[String] and
    (__ \ "common_words").formatNullable[Seq[String]] and
    (__ \ "common_words_path").formatNullable[String] and
    formatWithDefault(__ \ "ignore_case", defaultIgnoreCase) and
    formatWithDefault(__ \ "query_mode", defaultQueryMode)
  )(CommonGramsTokenFilter.apply, unlift(CommonGramsTokenFilter.unapply))
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-normalization-tokenfilter.html
 * Language can be arabic or persian.
 */
case class NormalizationTokenFilter(
    name: String,
    language: String
) extends TokenFilter
object NormalizationTokenFilter {
  def reads(language: String): Reads[NormalizationTokenFilter] = Reads {
    json: JsValue => JsSuccess(NormalizationTokenFilter((json \ "name").as[String], language))
  }
  implicit val writes: Writes[NormalizationTokenFilter] = Writes {
    filter: NormalizationTokenFilter => Json.obj("name" -> filter.name)
  }
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-keep-words-tokenfilter.html
 */
case class KeepWordsTokenFilter(
    name: String,
    keepWords: Option[Seq[String]] = None,
    keepWordsPath: Option[String] = None,
    keepWordsCase: Boolean = KeepWordsTokenFilter.defaultKeepWordsCase
) extends TokenFilter
object KeepWordsTokenFilter extends JsonUtils {
  val defaultKeepWordsCase = false
  implicit val format: Format[KeepWordsTokenFilter] = (
    (__ \ "name").format[String] and
    (__ \ "keep_words").formatNullable[Seq[String]] and
    (__ \ "keep_words_path").formatNullable[String] and
    formatWithDefault(__ \ "keep_words_case", defaultKeepWordsCase)
  )(KeepWordsTokenFilter.apply, unlift(KeepWordsTokenFilter.unapply))
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-delimited-payload-tokenfilter.html
 */
case class DelimitedPayloadTokenFilter(
    name: String,
    delimiter: String = DelimitedPayloadTokenFilter.defaultDelimiter,
    encoding: String = DelimitedPayloadTokenFilter.defaultEncoding
) extends TokenFilter
object DelimitedPayloadTokenFilter extends JsonUtils {
  val defaultDelimiter = "|"
  val defaultEncoding = "float"
  implicit val format: Format[DelimitedPayloadTokenFilter] = (
    (__ \ "name").format[String] and
    formatWithDefault(__ \ "delimiter", defaultDelimiter) and
    formatWithDefault(__ \ "encoding", defaultEncoding)
  )(DelimitedPayloadTokenFilter.apply, unlift(DelimitedPayloadTokenFilter.unapply))
}


/**
 * TokenFilter formats, reads and writes lists of filter definitions.
 */
object TokenFilter extends JsonUtils {
  import Analysis.{fieldFor, fromFields}

  implicit lazy val filterListReads: Reads[Seq[TokenFilter]] = Reads { json =>
    val filters =
      fromFields(StemmerTokenFilter.normalizeJson(json)) map { case (component, typeName) =>
        typeName match {
          case "standard" => StandardTokenFilter.format.reads(component)
          case "asciifolding" => AsciiFoldingTokenFilter.format.reads(component)
          case "length" => LengthTokenFilter.format.reads(component)
          case "lowercase" => LowercaseTokenFilter.format.reads(component)
          case "nGram" => NGramTokenFilter.format.reads(component)
          case "edgeNGram" => EdgeNGramTokenFilter.format.reads(component)
          case "porter_stem" => PorterStemTokenFilter.format.reads(component)
          case "shingle" => ShingleTokenFilter.format.reads(component)
          case "stop" => StopTokenFilter.format.reads(component)
          case "word_delimiter" => WordDelimiterTokenFilter.format.reads(component)
          case "stemmer" => StemmerTokenFilter.format.reads(component)
          case "stemmer_override" => StemmerOverrideTokenFilter.format.reads(component)
          case "keyword_marker" => KeywordMarkerTokenFilter.format.reads(component)
          case "keyword_repeat" => KeywordRepeatTokenFilter.format.reads(component)
          case "kstem" => KStemTokenFilter.format.reads(component)
          case "snowball" => SnowballTokenFilter.format.reads(component)
          case "phonetic" => PhoneticTokenFilter.format.reads(component)
          case "synonym" => SynonymTokenFilter.format.reads(component)
          case "dictionary_decompounder" => DictionaryDecompounderTokenFilter.format.reads(component)
          case "hyphenation_decompounder" => HyphenationDecompounderTokenFilter.format.reads(component)
          case "reverse" => ReverseTokenFilter.format.reads(component)
          case "elision" => ElisionTokenFilter.format.reads(component)
          case "truncate" => TruncateTokenFilter.format.reads(component)
          case "unique" => UniqueTokenFilter.format.reads(component)
          case "pattern_capture" => PatternCaptureTokenFilter.format.reads(component)
          case "pattern_replace" => PatternReplaceTokenFilter.format.reads(component)
          case "trim" => TrimTokenFilter.format.reads(component)
          case "limit" => LimitTokenCountTokenFilter.format.reads(component)
          case "hunspell" => HunspellTokenFilter.format.reads(component)
          case "common_grams" => CommonGramsTokenFilter.format.reads(component)
          case "arabic_normalization" => NormalizationTokenFilter.reads("arabic").reads(component)
          case "persian_normalization" => NormalizationTokenFilter.reads("persian").reads(component)
          case "keep" => KeepWordsTokenFilter.format.reads(component)
          case "delimited_payload_filter" => DelimitedPayloadTokenFilter.format.reads(component)
          case other => JsError("Unknown filter type [" + other + "] in json: " + json)
        }
      }
    foldJsResults(filters)
  }

  implicit lazy val filterListWrites: Writes[Seq[TokenFilter]] = Writes({filters: Seq[TokenFilter] =>
    JsObject(
      filters map {
        case f: StandardTokenFilter => fieldFor(f, "standard")
        case f: AsciiFoldingTokenFilter => fieldFor(f, "asciifolding")
        case f: LengthTokenFilter => fieldFor(f, "length")
        case f: LowercaseTokenFilter => fieldFor(f, "lowercase")
        case f: NGramTokenFilter => fieldFor(f, "nGram")
        case f: EdgeNGramTokenFilter => fieldFor(f, "edgeNGram")
        case f: PorterStemTokenFilter => fieldFor(f, "porter_stem")
        case f: ShingleTokenFilter => fieldFor(f, "shingle")
        case f: StopTokenFilter => fieldFor(f, "stop")
        case f: WordDelimiterTokenFilter => fieldFor(f, "word_delimiter")
        case f: StemmerTokenFilter => fieldFor(f, "stemmer")
        case f: StemmerOverrideTokenFilter => fieldFor(f, "stemmer_override")
        case f: KeywordMarkerTokenFilter => fieldFor(f, "keyword_marker")
        case f: KeywordRepeatTokenFilter => fieldFor(f, "keyword_repeat")
        case f: KStemTokenFilter => fieldFor(f, "kstem")
        case f: SnowballTokenFilter => fieldFor(f, "snowball")
        case f: PhoneticTokenFilter => fieldFor(f, "phonetic")
        case f: SynonymTokenFilter => fieldFor(f, "synonym")
        case f: DictionaryDecompounderTokenFilter => fieldFor(f, "dictionary_decompounder")
        case f: HyphenationDecompounderTokenFilter => fieldFor(f, "hyphenation_decompounder")
        case f: ReverseTokenFilter => fieldFor(f, "reverse")
        case f: ElisionTokenFilter => fieldFor(f, "elision")
        case f: TruncateTokenFilter => fieldFor(f, "truncate")
        case f: UniqueTokenFilter => fieldFor(f, "unique")
        case f: PatternCaptureTokenFilter => fieldFor(f, "pattern_capture")
        case f: PatternReplaceTokenFilter => fieldFor(f, "pattern_replace")
        case f: TrimTokenFilter => fieldFor(f, "trim")
        case f: LimitTokenCountTokenFilter => fieldFor(f, "limit")
        case f: HunspellTokenFilter => fieldFor(f, "hunspell")
        case f: CommonGramsTokenFilter => fieldFor(f, "common_grams")
        case f: NormalizationTokenFilter => fieldFor(f, f.language+"_normalization")
        case f: KeepWordsTokenFilter => fieldFor(f, "keep")
        case f: DelimitedPayloadTokenFilter => fieldFor(f, "delimited_payload_filter")
        case x => throw new IllegalArgumentException("No filterListWrites defined for "+x.getClass())
      }
    )
  })

}