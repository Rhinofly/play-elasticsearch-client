package play.modules.elasticsearch.filter

import play.api.libs.json.{Json, JsString, JsValue, Writes}
import play.modules.elasticsearch.JsonUtils

/*
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-range-filter.html
 *     http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-numeric-range-filter.html
 * This version supports the execution option, although that is not supported in the current stable version of ElasticSearch yet.
 * The execution option is translated to work with versions of ElasticSearch before 0.90.8.
 */
case class RangeFilter[BoundType : Writes](
  field: String,
  from: BoundType,
  to: BoundType,
  include_lower: Boolean = true,
  include_upper: Boolean = true,
  execution: RangeExecution.Value = RangeExecution.default
) extends Filter with JsonUtils {

  def toQueryDSL = {
    val filterType = if (execution == RangeExecution.fielddata) "numeric_range" else "range"
    Json.obj(filterType ->
      Json.obj(field ->
        Json.obj(
          (if (include_lower) "gte" else "gt") -> Json.toJson(from),
          (if (include_upper) "lte" else "lt") -> Json.toJson(to)
        )
      )
    )
  }

}
