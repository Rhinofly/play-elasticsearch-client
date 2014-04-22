package fly.play.elasticsearch.query

import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import fly.play.elasticsearch.JsonUtils

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-ids-query.html
 */
case class IdsQuery(
    types: Seq[String] = Seq.empty,
    values: Seq[String]
) extends Query with JsonUtils {

    def toQueryDSL =
    Json.obj("ids" ->
      toJsonObject(
        "type" -> toJsonIfNot(types, Seq.empty),
        "values" -> Json.toJson(values)
    ))

}