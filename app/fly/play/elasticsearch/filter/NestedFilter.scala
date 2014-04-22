package fly.play.elasticsearch.filter

import play.api.libs.json.{Json, JsString}
import fly.play.elasticsearch.JsonUtils

/*
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-nested-filter.html
 */
case class NestedFilter(
    path: String,
    filter: Filter,
    cache: Boolean = NestedFilter.defaultCache,
    join: Boolean = NestedFilter.defaultJoin
) extends Filter with JsonUtils {

  def toQueryDSL =
    Json.obj( "nested" ->
      toJsonObject(
        "path" -> JsString(path),
        "filter" -> filter.toQueryDSL,
        "_cache" -> toJsonIfNot(cache, NestedFilter.defaultCache),
        "join" -> toJsonIfNot(join, NestedFilter.defaultJoin)
      )
    )

}

object NestedFilter {
  val defaultCache = false
  val defaultJoin = true
}