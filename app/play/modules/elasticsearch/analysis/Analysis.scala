package play.modules.elasticsearch.analysis

import play.api.libs.functional.syntax.{functionalCanBuildApplicative, toFunctionalBuilderOps, unlift}
import play.api.libs.json.{Format, JsError, JsObject, JsResult, JsString, JsUndefined, JsValue, Reads, Writes, __}
import play.modules.elasticsearch.JsonUtils
import play.api.libs.json.JsUndefined
import scala.util.Try
import play.api.libs.json.JsSuccess
import play.api.libs.json.JsArray

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/analysis.html :
 * The Analysis module acts as a configurable registry of Analyzers that can be used in order
 * to both break indexed (analyzed) fields when a document is indexed and process query strings.
 * Analyzers are composed of a single Tokenizer and zero or more TokenFilters.
 * The tokenizer may be preceded by one or more CharFilters.
 * The Analysis module allows one to register CharFilters, TokenFilters, Tokenizers and Analyzers
 * under logical names that can then be referenced either in mapping definitions or in certain APIs.
 *
 * Currently we define components (analyzers, tokenizers, tokenfilters, charfilters) that are listed in the ElasticSearch reference (0.90).
 * The code specifying which analysis components are available can be found at https://github.com/elasticsearch/elasticsearch/blob/master/src/main/java/org/elasticsearch/index/analysis/AnalysisModule.java
 */
case class Analysis(
    analyzers: Seq[Analyzer] = Seq.empty,
    tokenizers: Seq[Tokenizer] = Seq.empty,
    filters: Seq[TokenFilter] = Seq.empty,
    charFilters: Seq[CharFilter] = Seq.empty
  )

/**
 * Analysis formats
 */
object Analysis extends JsonUtils {

  /* Format for Analysis, used in Settings. */
  implicit lazy val analysisFormat: Format[Analysis] = (
    (__ \ "analyzer").format[Seq[Analyzer]] and
    (__ \ "tokenizer").format[Seq[Tokenizer]] and
    (__ \ "filter").format[Seq[TokenFilter]] and
    (__ \ "char_filter").format[Seq[CharFilter]]
  )(Analysis.apply, unlift(Analysis.unapply))

  /**
   * Some components use mappings, which are sequences of strings of the form "a => b".
   */
  type Mappings = Seq[(String, String)]
  implicit val mappingsFormat = new Format[Mappings] {
    def reads(jsValue: JsValue) = jsValue match {
      case JsArray(mappings) => foldJsResults(mappings map {
        case JsString(value) if value.contains("=>") => {
          val parts = value.split("""\s*=>\s*""")
          JsSuccess((parts(0), parts(1)))
        }
        case other => JsError("Not a mapping: "+other)
      })
      case other => JsError("Not a mappings array: "+other)
    }
    def writes(mappings: Mappings) = JsArray(mappings map {mapping => JsString(mapping._1+"=>"+mapping._2)})
  }

  /**
   * Helper function that makes a JSON object-field (for use in Json.obj) for a Scala component definition.
   * For example, this turns an Analyzer component and type-name into "<name>" -> {"type": "<analyzer-type>", <analyzer-parameters>}.
   * This works for analyzers, tokenizers and (char)filters.
   */
  def fieldFor[T](component: T, typeName: String)(implicit writer: Writes[T]): (String, JsValue) = {
    // Create a field in the analyzer-object, mapping name to component.
    val json = writer.writes(component).as[JsObject]
    (json \ "name").as[String] -> (json - "name" +("type" -> JsString(typeName)))
  }

  /**
   * Helper function that does the reverse of fieldFor, for a sequence of fields.
   * For example, this turns JSON like {"<name1>" -> {"type": "<type1>", <parameters1>}, "<name2>" -> {"type": "<type2>", <parameters2>}, ...}
   * into a sequence of (component, typeName) tuples.
   * It is possible to accept missing type-fields if acceptUndefined. The typeName will then be "".
   */
  def fromFields(json: JsValue, acceptUndefined: Boolean = false): Seq[(JsObject, String)] = {
    json.as[JsObject].fields map { case(name, js) =>
      lazy val component = js.as[JsObject] + ("name" -> JsString(name))
      val typeName = (js \ "type") match {
        case JsString(typeName) => typeName
        case JsUndefined(_) if acceptUndefined => ""
        case JsUndefined(error) => error
        case _ => "Error: cannot read analysis component fields from "+json
      }
      (component, typeName)
    }
  }

}
