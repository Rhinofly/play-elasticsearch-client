package play.modules.elasticsearch.filter

import play.api.libs.json.Json
import play.api.libs.json.JsArray

/*
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-not-filter.html
 */
case class NotFilter(filter: Filter) extends Filter {

  def toQueryDSL =
    Json.obj("not" -> filter.toQueryDSL)

}
