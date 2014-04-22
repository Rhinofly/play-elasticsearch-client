package fly.play.elasticsearch.query

import play.api.libs.json.{JsArray, JsError, JsObject, JsSuccess, JsValue, Json}
import play.api.libs.json.{Reads, Writes}
import play.api.libs.json.JsObject
import play.api.libs.json.JsNull
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import fly.play.elasticsearch.{EnumUtils, JsonUtils}

/**
 * Highlighting.
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-request-highlighting.html
 * The `tags` sequence contains pairs of `pre_tag` and `post_tag`.
 */
case class Highlight(
  fields: Seq[HighlightField],
  tags: Option[Seq[(String, String)]] = None,
  tagsSchema: Option[String] = None,
  encoder: Option[String] = None,
  order: Option[String] = None,
  /* Global settings. */
  highlightType: HighlightType.Value = HighlightType.default,
  fragmentSize: Int = HighlightField.defaultFragmentSize,
  numberOfFragments: Int = HighlightField.defaultNumberOfFragments,
  fragmentOffset: Int = HighlightField.defaultFragmentOffset,
  matchedFields: Seq[String] = Seq.empty,
  highlightQuery: Option[Query] = None,
  requireFieldMatch: Boolean = HighlightField.defaultRequireFieldMatch,
  boundaryChars: String = HighlightField.defaultBoundaryChars,
  boundaryMaxScan: Int = HighlightField.defaultBoundaryMaxScan,
  phraseLimit: Int = HighlightField.defaultPhraseLimit
)

object Highlight extends JsonUtils {

  implicit val writes = new Writes[Highlight] {
    def writes(hilite: Highlight): JsValue = toJsonObject(
        "fields" -> JsObject(hilite.fields.flatMap(HighlightField.writes.writes(_).fields)),
        "pre_tags" -> toJsonIfNot(hilite.tags, None, {tags: Option[Seq[(String, String)]] => tags.get.map(_._1)}),
        "post_tags" -> toJsonIfNot(hilite.tags, None, {tags: Option[Seq[(String, String)]] => tags.get.map(_._2)}),
        "tags_schema" -> toJsonIfNot(hilite.tagsSchema, None),
        "encoder" -> toJsonIfNot(hilite.encoder, None), // Encoder does not seem to work as documented.
        "order" -> toJsonIfNot(hilite.order, None),
        /* Global settings. */
        "type" -> toJsonIfNot(hilite.highlightType.toString, HighlightType.default.toString),
        "fragment_size" -> toJsonIfNot(hilite.fragmentSize, HighlightField.defaultFragmentSize),
        "number_of_fragments" -> toJsonIfNot(hilite.numberOfFragments, HighlightField.defaultNumberOfFragments),
        "fragment_offset" -> toJsonIfNot(hilite.fragmentOffset, HighlightField.defaultFragmentOffset),
        "matched_fields" -> toJsonIfNot(hilite.matchedFields, Seq.empty),
        "highlight_query" -> toJsonIfNot(hilite.highlightQuery, None, {hlq: Option[Query] => hlq.get.toQueryDSL}),
        "require_field_match" -> toJsonIfNot(hilite.requireFieldMatch, HighlightField.defaultRequireFieldMatch),
        "boundary_chars" -> toJsonIfNot(hilite.boundaryChars, HighlightField.defaultBoundaryChars),
        "boundary_max_scan" -> toJsonIfNot(hilite.boundaryMaxScan, HighlightField.defaultBoundaryMaxScan),
        "phrase_limit" -> toJsonIfNot(hilite.phraseLimit, HighlightField.defaultPhraseLimit)
      )
  }

}

case class HighlightField(
  field: String,
  highlightType: HighlightType.Value = HighlightType.default,
  fragmentSize: Int = HighlightField.defaultFragmentSize,
  numberOfFragments: Int = HighlightField.defaultNumberOfFragments,
  fragmentOffset: Int = HighlightField.defaultFragmentOffset,
  matchedFields: Seq[String] = Seq.empty,
  highlightQuery: Option[Query] = None,
  requireFieldMatch: Boolean = HighlightField.defaultRequireFieldMatch,
  boundaryChars: String = HighlightField.defaultBoundaryChars,
  boundaryMaxScan: Int = HighlightField.defaultBoundaryMaxScan,
  phraseLimit: Int = HighlightField.defaultPhraseLimit
)

object HighlightField extends JsonUtils {

  val defaultFragmentSize = 100
  val defaultNumberOfFragments = 5
  val defaultFragmentOffset = 0
  val defaultRequireFieldMatch = false
  val defaultBoundaryChars = ".,!? \t\n"
  val defaultBoundaryMaxScan = 20
  val defaultPhraseLimit = 256

  implicit val writes = new Writes[HighlightField] {
    def writes(hilite: HighlightField): JsObject = Json.obj(
      hilite.field -> toJsonObject(
        "type" -> toJsonIfNot(hilite.highlightType.toString, HighlightType.default.toString),
        "fragment_size" -> toJsonIfNot(hilite.fragmentSize, defaultFragmentSize),
        "number_of_fragments" -> toJsonIfNot(hilite.numberOfFragments, defaultNumberOfFragments),
        "fragment_offset" -> toJsonIfNot(hilite.fragmentOffset, defaultFragmentOffset),
        "matched_fields" -> toJsonIfNot(hilite.matchedFields, Seq.empty),
        "highlight_query" -> toJsonIfNot(hilite.highlightQuery, None, {hlq: Option[Query] => hlq.get.toQueryDSL}),
        "require_field_match" -> toJsonIfNot(hilite.requireFieldMatch, defaultRequireFieldMatch),
        "boundary_chars" -> toJsonIfNot(hilite.boundaryChars, defaultBoundaryChars),
        "boundary_max_scan" -> toJsonIfNot(hilite.boundaryMaxScan, defaultBoundaryMaxScan),
        "phrase_limit" -> toJsonIfNot(hilite.phraseLimit, defaultPhraseLimit)
      )
    )
  }

}

/*
 * The default highlighter type depends on the index options of the mapping for a field.
 */
object HighlightType extends Enumeration {
  val default, plain, postings, fvh = Value
  implicit val enumReads: Reads[Value] = EnumUtils.enumReads(HighlightType)
  implicit val enumWrites: Writes[Value] = EnumUtils.enumWrites
}
