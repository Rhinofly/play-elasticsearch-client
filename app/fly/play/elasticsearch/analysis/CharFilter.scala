package fly.play.elasticsearch.analysis

import play.api.libs.functional.syntax._
import play.api.libs.json.{Format, JsError, JsObject, JsResult, JsString, JsSuccess, JsUndefined, JsValue, Reads, Writes, __}
import fly.play.elasticsearch.utils.JsonUtils
import scala.util.Try

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-charfilters.html
 */
sealed trait CharFilter

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-mapping-charfilter.html
 */
case class MappingCharFilter(
    name: String,
    mappings: Option[Analysis.Mappings] = None,
    mappingsPath: Option[String] = None
) extends CharFilter

object MappingCharFilter {
  import Analysis.mappingsFormat
  implicit val format: Format[MappingCharFilter] = (
    (__ \ "name").format[String] and
    (__ \ "mappings").formatNullable[Analysis.Mappings] and
    (__ \ "mappings_path").formatNullable[String]
    )(MappingCharFilter.apply, unlift(MappingCharFilter.unapply))
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-htmlstrip-charfilter.html
 */
case class HtmlStripCharFilter(name: String) extends CharFilter
object HtmlStripCharFilter extends JsonUtils {
  implicit val format: Format[HtmlStripCharFilter] = (__ \ "name").format[String].inmap(HtmlStripCharFilter(_), _.name)
}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis-pattern-replace-charfilter.html
 */
case class PatternReplaceCharFilter(
    name: String,
    pattern: String,
    replacement: String
) extends CharFilter

object PatternReplaceCharFilter {
  implicit val format: Format[PatternReplaceCharFilter] = (
    (__ \ "name").format[String] and
    (__ \ "pattern").format[String] and
    (__ \ "replacement").format[String]
    )(PatternReplaceCharFilter.apply, unlift(PatternReplaceCharFilter.unapply))
}

/**
 * CharFilter formats, reads and writes lists of charfilter definitions.
 */
object CharFilter extends JsonUtils {
  import Analysis.{fieldFor, fromFields}

  implicit lazy val filterListReads: Reads[Seq[CharFilter]] = Reads { json =>
    val filters =
      fromFields(json) map { case (component, typeName) =>
        typeName match {
          case "mapping" => MappingCharFilter.format.reads(component)
          case "html_strip" => HtmlStripCharFilter.format.reads(component)
          case "pattern_replace" => PatternReplaceCharFilter.format.reads(component)
          case other => JsError("Unknown charfilter type [" + other + "] in json: " + json)
        }
      }
    foldJsResults(filters)
  }

  implicit lazy val filterListWrites: Writes[Seq[CharFilter]] = Writes({filters: Seq[CharFilter] =>
    JsObject(
      filters map {
        case f: MappingCharFilter => fieldFor(f, "mapping")
        case f: HtmlStripCharFilter => fieldFor(f, "html_strip")
        case f: PatternReplaceCharFilter => fieldFor(f, "pattern_replace")
        case x => throw new IllegalArgumentException("No filterListWrites defined for "+x.getClass())
      }
    )
  })

}