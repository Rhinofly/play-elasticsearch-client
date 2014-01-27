package play.modules.elasticsearch.mapping

import play.api.libs.json._
import play.modules.elasticsearch.JsonUtils

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/mapping-core-types.html#boolean
 */
case class BooleanMapping(
    field: String,
    store: StoreType.Value = StoreType.default,
    index: IndexType.Value = IndexType.default,
    boost: BoostType.Value = BoostType.default,
    nullValue: Option[String] = None,
    includeInAll: Option[Boolean] = None
  ) extends NestableMapping with JsonUtils {

  def toJson: JsObject = Json.obj(field -> toJsonObject(
    "type" -> JsString(BooleanMapping.typeName),
    "store" -> toJsonIfNot(store, StoreType.default),
    "index" -> toJsonIfNot(index, IndexType.default),
    "boost" -> toJsonIfNot(boost, BoostType.default),
    "null_value" -> toJsonIfNot(nullValue, None),
    "include_in_all" -> toJsonIfNot(includeInAll, None)
  ))

}

object BooleanMapping extends MappingType {

  val typeName = "boolean"

  val defaultFormat = "dateOptionalTime"

  override def fromJson(field: (String, JsValue)) = field match {case (fieldName, jsMapping) =>
    BooleanMapping(
      fieldName,
      store = (jsMapping \ "store").asOpt[StoreType.Value].getOrElse(StoreType.default),
      index = (jsMapping \ "index").asOpt[IndexType.Value].getOrElse(IndexType.default),
      boost = (jsMapping \ "boost").asOpt[BoostType.Value].getOrElse(BoostType.default),
      nullValue = (jsMapping \ "null_value").asOpt[String],
      includeInAll = (jsMapping \ "include_in_all").asOpt[Boolean]
    )
  }

}
