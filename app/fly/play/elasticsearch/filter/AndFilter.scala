package fly.play.elasticsearch.filter

import play.api.libs.json.Json
import play.api.libs.json.JsArray

/*
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-and-filter.html
 */
case class AndFilter(filters: Seq[Filter]) extends Filter {

  def toQueryDSL =
    Json.obj("and" -> JsArray(filters.map{_.toQueryDSL}))

}
