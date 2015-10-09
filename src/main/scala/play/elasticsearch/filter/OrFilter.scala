package fly.play.elasticsearch.filter

import play.api.libs.json.Json
import play.api.libs.json.JsArray

/*
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-or-filter.html
 */
case class OrFilter(filters: Seq[Filter]) extends Filter {

  def toQueryDSL =
    Json.obj("or" -> JsArray(filters.map{_.toQueryDSL}))

}
