package play.modules.elasticsearch.mapping

import play.api.libs.json._
import play.modules.elasticsearch.JsonUtils

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/mapping-core-types.html#token_count
 */
case class TokenCountMapping(
    field: String,
    store: StoreType.Value = StoreType.default,
    index: IndexType.Value = IndexType.default,
    precisionStep: Int = NumberMapping.defaultPrecisionStep,
    boost: BoostType.Value = BoostType.default,
    nullValue: Option[String] = None,
    includeInAll: Option[Boolean] = None,
    ignoreMalformed: Boolean = NumberMapping.defaultIgnoreMalformed,
    analyzer: AnalyzerType.Value = AnalyzerType.default
  ) extends NestableMapping with JsonUtils {

  def toJson: JsObject = Json.obj(field -> toJsonObject(
    "type" -> JsString(TokenCountMapping.typeName),
    "store" -> toJsonIfNot(store, StoreType.default),
    "index" -> toJsonIfNot(index, IndexType.default),
    "precision_step" -> toJsonIfNot(precisionStep, NumberMapping.defaultPrecisionStep),
    "boost" -> toJsonIfNot(boost, BoostType.default),
    "null_value" -> toJsonIfNot(nullValue, None),
    "include_in_all" -> toJsonIfNot(includeInAll, None),
    "ignore_malformed" -> toJsonIfNot(ignoreMalformed, NumberMapping.defaultIgnoreMalformed),
    "analyzer" -> toJsonIfNot(analyzer, AnalyzerType.default)
  ))

}

object TokenCountMapping extends MappingType {

  val typeName = "token_count"

  override def fromJson(field: (String, JsValue)) = field match {case (fieldName, jsMapping) =>
    TokenCountMapping(
      fieldName,
      store = (jsMapping \ "store").asOpt[StoreType.Value].getOrElse(StoreType.default),
      index = (jsMapping \ "index").asOpt[IndexType.Value].getOrElse(IndexType.default),
      precisionStep = (jsMapping \ "precision_step").asOpt[Int].getOrElse(NumberMapping.defaultPrecisionStep),
      boost = (jsMapping \ "boost").asOpt[BoostType.Value].getOrElse(BoostType.default),
      nullValue = (jsMapping \ "null_value").asOpt[String],
      includeInAll = (jsMapping \ "include_in_all").asOpt[Boolean],
      ignoreMalformed = (jsMapping \ "ignore_malformed").asOpt[Boolean].getOrElse(NumberMapping.defaultIgnoreMalformed),
      analyzer = (jsMapping \ "analyzer").asOpt[AnalyzerType.Value].getOrElse(AnalyzerType.default)
    )
  }

}
