package play.modules.elasticsearch.query

import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.libs.json.Writes
import play.modules.elasticsearch.JsonUtils

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-range-query.html
 */
case class RangeQuery[BoundType : Writes](
  field: String,
  from: BoundType,
  to: BoundType,
  includeLower: Boolean = true,
  includeUpper: Boolean = true,
  boost: Double = 1.0
) extends Query with JsonUtils {

  def toQueryDSL = {
    Json.obj("range" ->
      Json.obj(field ->
        toJsonObject(
          (if (includeLower) "gte" else "gt") -> Json.toJson(from),
          (if (includeUpper) "lte" else "lt") -> Json.toJson(to),
          "boost" -> toJsonIfNot(boost, 1.0)
        )
      )
    )
  }

}
