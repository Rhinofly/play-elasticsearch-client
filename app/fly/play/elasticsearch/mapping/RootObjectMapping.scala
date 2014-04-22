package fly.play.elasticsearch.mapping

import play.api.libs.json.{JsObject, JsString, JsValue, Json}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import fly.play.elasticsearch.{ElasticSearchException, JsonUtils}
import play.api.libs.json.JsNull

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/mapping-root-object-type.html
 * This Mapping must only be used at the top-level of a mapping tree, in other words it must not be contained within an ObjectMapping's properties.
 *TODO: dynamic_templates
 */
case class RootObjectMapping(mapping: Mapping, rootProperties: RootProperties) extends Mapping {

  def field = mapping.field

  /**
   * Make a JSON representation. Must return a JsObject("field" -> JsObject(...)).
   */
  override def toJson: JsObject =
    mapping.toJson match {
      case JsObject(Seq((field, definition: JsObject))) => JsObject(Seq((field, definition ++ rootProperties.toJson)))
    }

  override def withFieldData(fieldData: FieldData) =
    new RootObjectMapping(mapping.withFieldData(fieldData), rootProperties)

}

object RootObjectMapping {

  private val rootFields = Set("analyzer", "index_analyzer", "search_analyzer", "dynamic_date_formats", "date_detection", "numeric_detection")

  /**
   * Add to a mapping from a field-specification
   * @param fieldSpec The field-specification, at the level where it has a "type" field.
   * @param mapping The mapping that will be augmented if the field specification has root properties.
   */
  def addToFromJson(fieldSpec: JsValue, mapping: Mapping): Mapping =
    fieldSpec match {
      case spec: JsObject if (spec.keys & rootFields).size > 0 => RootObjectMapping(mapping, RootProperties.fromJson(spec))
      case _ => mapping
    }

}

case class RootProperties(
    analyzer: Option[String] = None,
    indexAnalyzer: Option[String] = None,
    searchAnalyzer: Option[String] = None,
    dynamicDateFormats: Option[Seq[String]] = None,
    dateDetection: Boolean = RootProperties.defaultDateDetection,
    numericDetection: Boolean = RootProperties.defaultNumericDetection
  ) extends JsonUtils {

  def toJson: JsObject = toJsonObject(
    "analyzer" -> toJsonIfNot(analyzer, None),
    "index_analyzer" -> toJsonIfNot(indexAnalyzer, None),
    "search_analyzer" -> toJsonIfNot(searchAnalyzer, None),
    "dynamic_date_formats" -> toJsonIfNot(dynamicDateFormats, None),
    "date_detection" -> toJsonIfNot(dateDetection, RootProperties.defaultDateDetection),
    "numeric_detection" -> toJsonIfNot(numericDetection, RootProperties.defaultNumericDetection)
  )

}

object RootProperties {

  val defaultDateDetection = true
  val defaultNumericDetection = false

  def fromJson(properties: JsObject) = RootProperties(
    analyzer = (properties \ "analyzer").asOpt[String],
    indexAnalyzer = (properties \ "index_analyzer").asOpt[String],
    searchAnalyzer = (properties \ "search_analyzer").asOpt[String],
    dynamicDateFormats = (properties \ "dynamic_date_formats").asOpt[Seq[String]],
    dateDetection = (properties \ "date_detection").asOpt[Boolean].getOrElse(defaultDateDetection),
    numericDetection = (properties \ "numeric_detection").asOpt[Boolean].getOrElse(defaultNumericDetection)
  )

}
