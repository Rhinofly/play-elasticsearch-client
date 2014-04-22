package fly.play.elasticsearch.mapping

import play.api.libs.json._
import fly.play.elasticsearch.utils.JsonUtils

/**
 * http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/mapping-core-types.html#string
 */
case class StringMapping(
    field: String,
    store: StoreType.Value = StoreType.default,
    index: IndexType.Value = IndexType.default,
    termVector: TermVectorType.Value = TermVectorType.default,
    boost: BoostType.Value = BoostType.default,
    nullValue: Option[String] = None,
    analyzer: AnalyzerType.Value = AnalyzerType.default,
    indexAnalyzer: AnalyzerType.Value = AnalyzerType.default,
    searchAnalyzer: AnalyzerType.Value = AnalyzerType.default,
    includeInAll: Option[Boolean] = None
  ) extends NestableMapping with JsonUtils {

  override def toJson: JsObject = Json.obj(field -> toJsonObject(
      "type" -> JsString(StringMapping.typeName),
      "store" -> toJsonIfNot(store, StoreType.default),
      "index" -> toJsonIfNot(index, IndexType.default),
      "term_vector" -> toJsonIfNot(termVector, TermVectorType.default),
      "boost" -> toJsonIfNot(boost, BoostType.default),
      "null_value" -> toJsonIfNot(nullValue, None),
      "analyzer" -> toJsonIfNot(analyzer, AnalyzerType.default),
      "index_analyzer" -> toJsonIfNot(indexAnalyzer, AnalyzerType.default),
      "search_analyzer" -> toJsonIfNot(searchAnalyzer, AnalyzerType.default),
      "include_in_all" -> toJsonIfNot(includeInAll, None)
    ))

}

object StringMapping extends MappingType {

  val typeName = "string"

  override def fromJson(field: (String, JsValue)) = field match {case (fieldName, jsMapping) =>
    StringMapping(
      fieldName,
      store = (jsMapping \ "store").asOpt[StoreType.Value].getOrElse(StoreType.default),
      index = (jsMapping \ "index").asOpt[IndexType.Value].getOrElse(IndexType.default),
      termVector = (jsMapping \ "term_vector").asOpt[TermVectorType.Value].getOrElse(TermVectorType.default),
      boost = (jsMapping \ "boost").asOpt[BoostType.Value].getOrElse(BoostType.default),
      nullValue = (jsMapping \ "null_value").asOpt[String],
      analyzer = (jsMapping \ "analyzer").asOpt[AnalyzerType.Value].getOrElse(AnalyzerType.default),
      indexAnalyzer = (jsMapping \ "index_analyzer").asOpt[AnalyzerType.Value].getOrElse(AnalyzerType.default),
      searchAnalyzer = (jsMapping \ "search_analyzer").asOpt[AnalyzerType.Value].getOrElse(AnalyzerType.default),
      includeInAll = (jsMapping \ "include_in_all").asOpt[Boolean]
    )
  }

}
