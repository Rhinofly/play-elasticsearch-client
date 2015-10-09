package fly.play.elasticsearch.mapping

import fly.play.elasticsearch.utils.{EnumUtils, JsonUtils}
import play.api.libs.json.{JsObject, JsValue, Json, Reads, Writes}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/mapping-core-types.html#number
 */

case class NumberMapping(
    field: String,
    numberType: NumberType.Value,
    store: StoreType.Value = StoreType.default,
    index: IndexType.Value = IndexType.default,
    precisionStep: Int = NumberMapping.defaultPrecisionStep,
    boost: BoostType.Value = BoostType.default,
    nullValue: Option[String] = None,
    includeInAll: Option[Boolean] = None,
    ignoreMalformed: Boolean = NumberMapping.defaultIgnoreMalformed
  ) extends NestableMapping with JsonUtils {

  def toJson: JsObject = Json.obj(field -> toJsonObject(
    "type" -> Json.toJson(numberType),
    "store" -> toJsonIfNot(store, StoreType.default),
    "index" -> toJsonIfNot(index, IndexType.default),
    "precision_step" -> toJsonIfNot(precisionStep, NumberMapping.defaultPrecisionStep),
    "boost" -> toJsonIfNot(boost, BoostType.default),
    "null_value" -> toJsonIfNot(nullValue, None),
    "include_in_all" -> toJsonIfNot(includeInAll, None),
    "ignore_malformed" -> toJsonIfNot(ignoreMalformed, NumberMapping.defaultIgnoreMalformed)
  ))

}

object NumberMapping extends JsonUtils {
  val defaultPrecisionStep = 4
  val defaultIgnoreMalformed = false
  val float = new NumberMappingFor(NumberType.float)
  val double = new NumberMappingFor(NumberType.double)
  val integer = new NumberMappingFor(NumberType.integer)
  val long = new NumberMappingFor(NumberType.long)
  val short = new NumberMappingFor(NumberType.short)
  val byte = new NumberMappingFor(NumberType.byte)
}

object NumberType extends Enumeration {
  val float, double, integer, long, short, byte = Value
  implicit val enumReads: Reads[Value] = EnumUtils.enumReads(NumberType)
  implicit val enumWrites: Writes[Value] = EnumUtils.enumWrites
}

class NumberMappingFor(numberType: NumberType.Value) extends MappingType {
  val typeName = numberType.toString()
  override def fromJson(field: (String, JsValue)): NumberMapping = field match {case (fieldName, jsMapping) =>
    NumberMapping(
      fieldName,
      numberType = numberType,
      store = (jsMapping \ "store").asOpt[StoreType.Value].getOrElse(StoreType.default),
      index = (jsMapping \ "index").asOpt[IndexType.Value].getOrElse(IndexType.default),
      precisionStep = (jsMapping \ "precision_step").asOpt[Int].getOrElse(NumberMapping.defaultPrecisionStep),
      boost = (jsMapping \ "boost").asOpt[BoostType.Value].getOrElse(BoostType.default),
      nullValue = (jsMapping \ "null_value").asOpt[String],
      includeInAll = (jsMapping \ "include_in_all").asOpt[Boolean],
      ignoreMalformed = (jsMapping \ "ignore_malformed").asOpt[Boolean].getOrElse(NumberMapping.defaultIgnoreMalformed)
    )
  }
}
