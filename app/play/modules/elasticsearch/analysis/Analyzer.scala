package play.modules.elasticsearch.analysis

import play.api.libs.functional.syntax._
import play.api.libs.json.{Format, JsError, JsObject, JsPath, JsResult, JsString, JsUndefined, JsValue, Reads, Writes, __}
import play.modules.elasticsearch.JsonUtils

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-analyzers.html
 * ElasticSearch comes with a number of prebuilt analyzers which are ready to use.
 * Alternatively, you can combine the built in character filters, tokenizers and token filters to create custom analyzers.
 */
sealed trait Analyzer {
  def name: String
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-standard-analyzer.html
 * stopwords: Default is English.
 */
case class StandardAnalyzer(
    name: String,
    stopwords: Option[Seq[String]] = None,
    maxTokenLength: Int = StandardAnalyzer.defaultMaxTokenLength
  ) extends Analyzer

object StandardAnalyzer extends JsonUtils {
  val defaultMaxTokenLength = 255
  implicit val format: Format[StandardAnalyzer] = (
    (__ \ "name").format[String] and
    (__ \ "stopwords").formatNullable[Seq[String]] and
    formatWithDefault(__ \ "max_token_length", defaultMaxTokenLength)
    )(StandardAnalyzer.apply, unlift(StandardAnalyzer.unapply))
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-simple-analyzer.html
 * Maybe analyzers without parameters should be written as macros or using reflection?
 */
case class SimpleAnalyzer(name: String) extends Analyzer
object SimpleAnalyzer extends JsonUtils {
  implicit val format: Format[SimpleAnalyzer] = (__ \ "name").format[String].inmap(SimpleAnalyzer(_), _.name)
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-whitespace-analyzer.html
 */
case class WhitespaceAnalyzer(name: String) extends Analyzer
object WhitespaceAnalyzer extends JsonUtils {
  implicit val format: Format[WhitespaceAnalyzer] = (__ \ "name").format[String].inmap(WhitespaceAnalyzer(_), _.name)
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-stop-analyzer.html
 */
case class StopAnalyzer(
    name: String,
    stopwords: Option[Seq[String]] = None,
    stopwordsPath: Option[String] = None
  ) extends Analyzer

object StopAnalyzer {
  implicit val format: Format[StopAnalyzer] = (
    (__ \ "name").format[String] and
    (__ \ "stopwords").formatNullable[Seq[String]] and
    (__ \ "stopwords_path").formatNullable[String]
    )(StopAnalyzer.apply, unlift(StopAnalyzer.unapply))
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-keyword-analyzer.html
 */
case class KeywordAnalyzer(name: String) extends Analyzer
object KeywordAnalyzer extends JsonUtils {
  implicit val format: Format[KeywordAnalyzer] = (__ \ "name").format[String].inmap(KeywordAnalyzer(_), _.name)
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-pattern-analyzer.html
 * Flags follow the Java pattern API, as defined in http://docs.oracle.com/javase/6/docs/api/java/util/regex/Pattern.html#field_summary
 */
case class PatternAnalyzer(
    name: String,
    pattern: Option[String] = None,
    flags: Option[String] = None,
    lowercase: Boolean = PatternAnalyzer.defaultLowercase
  ) extends Analyzer

object PatternAnalyzer extends JsonUtils {
  val defaultLowercase = true
  implicit val format: Format[PatternAnalyzer] = (
    (__ \ "name").format[String] and
    (__ \ "pattern").formatNullable[String] and
    (__ \ "flags").formatNullable[String] and
    formatWithDefault(__ \ "lowercase", defaultLowercase)
    )(PatternAnalyzer.apply, unlift(PatternAnalyzer.unapply))
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-lang-analyzer.html
 * stemExclusion is a list of "protected" words, which are not stemmed. Not all languages support this.
 */
case class LanguageAnalyzer(
    name: String,
    language: String, // The selected language, called 'type' in Elasticsearch.
    stopwords: Option[Seq[String]] = None,
    stopwordsPath: Option[String] = None,
    stemExclusion: Option[Seq[String]] = None
) extends Analyzer {
  require(LanguageAnalyzer.languages.contains(language), "Unknown language: "+ language)
}

object LanguageAnalyzer {
  implicit val format: Format[LanguageAnalyzer] = (
    (__ \ "name").format[String] and
    (__ \ "type").format[String] and
    (__ \ "stopwords").formatNullable[Seq[String]] and
    (__ \ "stopwords_path").formatNullable[String] and
    (__ \ "stem_exclusion").formatNullable[Seq[String]]
    )(LanguageAnalyzer.apply, unlift(LanguageAnalyzer.unapply))
  lazy val languages = "arabic, armenian, basque, brazilian, bulgarian, catalan, chinese, cjk, czech, danish, dutch, english, finnish, french, galician, german, greek, hindi, hungarian, indonesian, italian, norwegian, persian, portuguese, romanian, russian, spanish, swedish, turkish, thai".
    split(",\\s*").toSet
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-snowball-analyzer.html
 */
case class SnowballAnalyzer(
    name: String,
    language: Option[String] = None, // defaults to "English"
    stopwords: Option[Seq[String]] = None
  ) extends Analyzer

object SnowballAnalyzer {
  implicit val format: Format[SnowballAnalyzer] = (
    (__ \ "name").format[String] and
    (__ \ "language").formatNullable[String] and
    (__ \ "stopwords").formatNullable[Seq[String]]
    )(SnowballAnalyzer.apply, unlift(SnowballAnalyzer.unapply))
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-custom-analyzer.html
 */
case class CustomAnalyzer(
    name: String,
    tokenizer: String,
    filter: Option[Seq[String]] = None,
    charFilter: Option[Seq[String]] = None
  ) extends Analyzer

object CustomAnalyzer {
  implicit val format: Format[CustomAnalyzer] = (
    (__ \ "name").format[String] and
    (__ \ "tokenizer").format[String] and
    (__ \ "filter").formatNullable[Seq[String]] and
    (__ \ "char_filter").formatNullable[Seq[String]]
    )(CustomAnalyzer.apply, unlift(CustomAnalyzer.unapply))
}

/**
 * Analyzer formats
 */
object Analyzer extends JsonUtils {
  import Analysis.{fieldFor, fromFields}

  /**
   * Reads for a list of analyzers.
   * The analyzers come in one JSON object, with analyzer-names as keys.
   * The exact parsing depends on the analyzer type, which is part of the JSON values to be parsed.
   */
  implicit lazy val analyzerListReads: Reads[Seq[Analyzer]] = Reads { json =>
    val analyzers =
      fromFields(json, acceptUndefined = true) map { case (component, typeName) =>
        typeName match {
          // From the ElasticSearch Server book, it appears that the type is optional for the custom analyzer, hence "".
          case "custom" | "" => CustomAnalyzer.format.reads(component)
          case "standard" => StandardAnalyzer.format.reads(component)
          case "simple" => SimpleAnalyzer.format.reads(component)
          case "whitespace" => WhitespaceAnalyzer.format.reads(component)
          case "stop" => StopAnalyzer.format.reads(component)
          case "keyword" => KeywordAnalyzer.format.reads(component)
          case "pattern" => PatternAnalyzer.format.reads(component)
          case "snowball" => SnowballAnalyzer.format.reads(component)
          // Language analyzers come last, because this involves a set-lookup.
          case language if LanguageAnalyzer.languages.contains(language) => LanguageAnalyzer.format.reads(component)
          case other => JsError("Unknown analyzer type: " + other) //TODO: Include a meaningful JsPath - how?
        }
      }
    foldJsResults(analyzers)
  }

  /**
   * Writes for a list of analyzers.
   */
  implicit lazy val analyzerListWrites: Writes[Seq[Analyzer]] = Writes({analyzers: Seq[Analyzer] =>
    JsObject(
      analyzers map {
        case a: CustomAnalyzer => fieldFor(a, "custom")
        case a: StandardAnalyzer => fieldFor(a, "standard")
        case a: SimpleAnalyzer => fieldFor(a, "simple")
        case a: WhitespaceAnalyzer => fieldFor(a, "whitespace")
        case a: StopAnalyzer => fieldFor(a, "stop")
        case a: KeywordAnalyzer => fieldFor(a, "keyword")
        case a: PatternAnalyzer => fieldFor(a, "pattern")
        case a: LanguageAnalyzer => fieldFor(a, a.language)
        case a: SnowballAnalyzer => fieldFor(a, "snowball")
        case x => throw new IllegalArgumentException("No Format defined for "+x.getClass())
      }
    )
  })

}