package fly.play.elasticsearch.analysis

import play.api.libs.functional.syntax._
import play.api.libs.json.{Format, JsError, JsObject, JsResult, JsString, JsUndefined, JsValue, Reads, Writes, __}
import fly.play.elasticsearch.JsonUtils

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-tokenizers.html
 */
sealed trait Tokenizer {
  def name: String
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-standard-tokenizer.html
 */
case class StandardTokenizer(
    name: String,
    maxTokenLength: Int = StandardTokenizer.defaultMaxTokenLength
  ) extends Tokenizer

object StandardTokenizer extends JsonUtils {
  val defaultMaxTokenLength = 255
  implicit val format: Format[StandardTokenizer] = (
    (__ \ "name").format[String] and
    formatWithDefault(__ \ "max_token_length", defaultMaxTokenLength)
    )(StandardTokenizer.apply, unlift(StandardTokenizer.unapply))
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-edgengram-tokenizer.html
 */
case class EdgeNGramTokenizer(
    name: String,
    minGram: Int = EdgeNGramTokenizer.defaultMinGram,
    maxGram: Int = EdgeNGramTokenizer.defaultMaxGram,
    tokenChars: Option[Seq[String]] = None
  ) extends Tokenizer

object EdgeNGramTokenizer extends JsonUtils {
  val defaultMinGram = 1
  val defaultMaxGram = 2
  implicit val format: Format[EdgeNGramTokenizer] = (
    (__ \ "name").format[String] and
    formatWithDefault(__ \ "min_gram", defaultMinGram) and
    formatWithDefault(__ \ "max_gram", defaultMaxGram) and
    (__ \ "token_chars").formatNullable[Seq[String]]
    )(EdgeNGramTokenizer.apply, unlift(EdgeNGramTokenizer.unapply))
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-keyword-tokenizer.html
 */
case class KeywordTokenizer(
    name: String,
    bufferSize: Int = KeywordTokenizer.defaultBufferSize
) extends Tokenizer

object KeywordTokenizer extends JsonUtils {
  val defaultBufferSize = 256
  implicit val format: Format[KeywordTokenizer] = (
    (__ \ "name").format[String] and
    formatWithDefault(__ \ "buffer_size", defaultBufferSize)
    )(KeywordTokenizer.apply, unlift(KeywordTokenizer.unapply))
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-lowercase-tokenizer.html
 */
case class LowercaseTokenizer(name: String) extends Tokenizer
object LowercaseTokenizer extends JsonUtils {
  implicit val format: Format[LowercaseTokenizer] = (__ \ "name").format[String].inmap(LowercaseTokenizer(_), _.name)
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-letter-tokenizer.html
 */
case class LetterTokenizer(name: String) extends Tokenizer
object LetterTokenizer extends JsonUtils {
  implicit val format: Format[LetterTokenizer] = (__ \ "name").format[String].inmap(LetterTokenizer(_), _.name)
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-ngram-tokenizer.html
 */
case class NGramTokenizer(
    name: String,
    minGram: Int = NGramTokenizer.defaultMinGram,
    maxGram: Int = NGramTokenizer.defaultMaxGram,
    tokenChars: Option[Seq[String]] = None
  ) extends Tokenizer

object NGramTokenizer extends JsonUtils {
  val defaultMinGram = 1
  val defaultMaxGram = 2
  implicit val format: Format[NGramTokenizer] = (
    (__ \ "name").format[String] and
    formatWithDefault(__ \ "min_gram", defaultMinGram) and
    formatWithDefault(__ \ "max_gram", defaultMaxGram) and
    (__ \ "token_chars").formatNullable[Seq[String]]
    )(NGramTokenizer.apply, unlift(NGramTokenizer.unapply))
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-whitespace-tokenizer.html
 */
case class WhitespaceTokenizer(name: String) extends Tokenizer
object WhitespaceTokenizer extends JsonUtils {
  implicit val format: Format[WhitespaceTokenizer] = (__ \ "name").format[String].inmap(WhitespaceTokenizer(_), _.name)
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-pattern-tokenizer.html
 */
case class PatternTokenizer(
    name: String,
    pattern: Option[String] = None,
    flags: Option[String] = None,
    group: Option[Int] = None
) extends Tokenizer

object PatternTokenizer extends JsonUtils {
  implicit val format: Format[PatternTokenizer] = (
    (__ \ "name").format[String] and
    (__ \ "pattern").formatNullable[String] and
    (__ \ "flags").formatNullable[String] and
    (__ \ "group").formatNullable[Int]
    )(PatternTokenizer.apply, unlift(PatternTokenizer.unapply))
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-uaxurlemail-tokenizer.html
 */
case class UaxUrlEmailTokenizer(
    name: String,
    maxTokenLength: Int = UaxUrlEmailTokenizer.defaultMaxTokenLength
  ) extends Tokenizer

object UaxUrlEmailTokenizer extends JsonUtils {
  val defaultMaxTokenLength = 255
  implicit val format: Format[UaxUrlEmailTokenizer] = (
    (__ \ "name").format[String] and
    formatWithDefault(__ \ "max_token_length", defaultMaxTokenLength)
    )(UaxUrlEmailTokenizer.apply, unlift(UaxUrlEmailTokenizer.unapply))
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-pathhierarchy-tokenizer.html
 */
case class PathHierarchyTokenizer(
    name: String,
    delimiter: String = PathHierarchyTokenizer.defaultDelimiter,
    replacement: Option[String] = None,
    bufferSize: Int = PathHierarchyTokenizer.defaultBufferSize,
    reverse: Boolean = PathHierarchyTokenizer.defaultReverse,
    skip: Option[Int] = None
  ) extends Tokenizer

object PathHierarchyTokenizer extends JsonUtils {
  val defaultDelimiter = "/"
  val defaultBufferSize = 1024
  val defaultReverse = false
  implicit val format: Format[PathHierarchyTokenizer] = (
    (__ \ "name").format[String] and
    formatWithDefault(__ \ "delimiter", defaultDelimiter) and
    (__ \ "replacement").formatNullable[String] and
    formatWithDefault(__ \ "buffer_size", defaultBufferSize) and
    formatWithDefault(__ \ "reverse", defaultReverse) and
    (__ \ "skip").formatNullable[Int]
    )(PathHierarchyTokenizer.apply, unlift(PathHierarchyTokenizer.unapply))
}

/**
 * Tokenizer formats
 */
object Tokenizer extends JsonUtils {
  import Analysis.{fieldFor, fromFields}

  /**
   * Read a list of tokenizers, analogously to analyzers
   */
  implicit lazy val tokenizerListReads: Reads[Seq[Tokenizer]] = Reads { json =>
    val tokenizers =
      fromFields(json) map { case (component, typeName) =>
        typeName match {
          case "standard" => StandardTokenizer.format.reads(component)
          case "edgeNGram" => EdgeNGramTokenizer.format.reads(component)
          case "keyword" => KeywordTokenizer.format.reads(component)
          case "letter" => LetterTokenizer.format.reads(component)
          case "lowercase" => LowercaseTokenizer.format.reads(component)
          case "nGram" => NGramTokenizer.format.reads(component)
          case "whitespace" => WhitespaceTokenizer.format.reads(component)
          case "pattern" => PatternTokenizer.format.reads(component)
          case "uax_url_email" => UaxUrlEmailTokenizer.format.reads(component)
          case "path_hierarchy" => PathHierarchyTokenizer.format.reads(component)
          case other => JsError("Unknown tokenizer type: " + other + " in json: " + json)
        }
      }
    foldJsResults(tokenizers)
  }

  /**
   * Writes for a list of tokenizers.
   */
  implicit lazy val tokenizerListWrites: Writes[Seq[Tokenizer]] = Writes({tokenizers: Seq[Tokenizer] =>
    JsObject(
      tokenizers map {
        case t: StandardTokenizer => fieldFor(t, "standard")
        case t: EdgeNGramTokenizer => fieldFor(t, "edgeNGram")
        case t: KeywordTokenizer => fieldFor(t, "keyword")
        case t: LetterTokenizer => fieldFor(t, "letter")
        case t: LowercaseTokenizer => fieldFor(t, "lowercase")
        case t: NGramTokenizer => fieldFor(t, "nGram")
        case t: WhitespaceTokenizer => fieldFor(t, "whitespace")
        case t: PatternTokenizer => fieldFor(t, "pattern")
        case t: UaxUrlEmailTokenizer => fieldFor(t, "uax_url_email")
        case t: PathHierarchyTokenizer => fieldFor(t, "path_hierarchy")
        case x => throw new IllegalArgumentException("No tokenizerListWrites defined for "+x.getClass())
      }
    )
  })

}