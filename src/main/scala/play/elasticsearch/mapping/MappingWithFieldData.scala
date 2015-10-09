package fly.play.elasticsearch.mapping

import play.api.libs.json.{JsNull, JsObject, JsValue, Json}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import fly.play.elasticsearch.utils.JsonUtils
import fly.play.elasticsearch.ElasticSearchException

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/mapping-core-types.html#fielddata-filters
 * A MappingWithFieldData extends any other mapping, but may do so only once.
 * This is particularly useful for StringMappings.
 */
class MappingWithFieldData(val mapping: Mapping, fieldData: FieldData) extends NestableMapping with JsonUtils {

  def field = mapping.field

  /**
   * Make a JSON representation. Must return a JsObject("field" -> JsObject(...)).
   */
  override def toJson: JsObject =
    mapping.toJson match {
      case JsObject(fields) if fields.size == 1 =>
        val (field, definition: JsObject) = fields.head
        JsObject(Map(field -> (definition + ("fielddata" -> fieldData.toJson))))
    }

  override def withFieldData(fieldData: FieldData): Mapping =
    throw ElasticSearchException(-1, "Cannot apply withFieldData to a MappingWithFieldData", JsNull)

}

object MappingWithFieldData {

  /**
   * Add to a mapping from a field-specification
   * @param fieldSpec The field-specification, at the level where it has a "type" field.
   * @param mapping The mapping that will be augmented if the field specification contains fielddata.
   */
  def addToFromJson(fieldSpec: JsValue, mapping: Mapping): Mapping =
    (fieldSpec \ "fielddata") match {
      case fielddata: JsObject => mapping.withFieldData(FieldData.fromJson(fielddata))
      case _ => mapping
    }

}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/mapping-core-types.html#fielddata-filters
 */
case class FieldData(filter: FieldDataFilter) {
  def toJson: JsObject = Json.obj("filter" -> filter.toJson)
}

object FieldData {
  /**
   * @param obj A JsObject of the form {filter: { ...}}
   */
  def fromJson(fielddata: JsObject) = (fielddata \ "filter") match {
    case filter: JsObject => FieldData(
        filter = FieldDataFilter.fromJson(filter)
      )
    case _ => throw ElasticSearchException(-1, "Fielddata must have a filter-field.", fielddata)
  }

}

case class FieldDataFilter(
    regexPattern: String = FieldDataFilter.defaultRegexPattern,
    frequencyMin: Double = FieldDataFilter.defaultFrequencyMin,
    frequencyMax: Double = FieldDataFilter.defaultFrequencyMax,
    minSegmentSize: Int = FieldDataFilter.defaultMinSegmentSize
  ) extends JsonUtils {

  def toJson: JsObject =
    toJsonObject(
      "regex" -> toJsonObject(
        "pattern" -> toJsonIfNot(regexPattern, FieldDataFilter.defaultRegexPattern)
      ),
      "frequency" -> toJsonObject(
        "min" -> toJsonIfNot(frequencyMin, FieldDataFilter.defaultFrequencyMin),
        "max" -> toJsonIfNot(frequencyMax, FieldDataFilter.defaultFrequencyMax),
        "min_segment_size" -> toJsonIfNot(minSegmentSize, FieldDataFilter.defaultMinSegmentSize)
      )
    )
}

object FieldDataFilter {
  val defaultRegexPattern = ""
  val defaultFrequencyMin = -1.0
  val defaultFrequencyMax = -1.0
  val defaultMinSegmentSize = 0

  def fromJson(filter: JsObject) = FieldDataFilter(
    regexPattern = (filter \ "regex" \ "pattern").asOpt[String].getOrElse(defaultRegexPattern),
    frequencyMin = (filter \ "frequency" \ "min").asOpt[Double].getOrElse(defaultFrequencyMin),
    frequencyMax = (filter \ "frequency" \ "max").asOpt[Double].getOrElse(defaultFrequencyMax),
    minSegmentSize = (filter \ "frequency" \ "min_segment_size").asOpt[Int].getOrElse(defaultMinSegmentSize)
  )

}
