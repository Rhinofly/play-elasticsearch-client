package fly.play.elasticsearch.filter

import play.api.libs.json.Json

/*
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-term-filter.html
 */
case class TermFilter(field: String, value: String) extends Filter {

  def toQueryDSL =
    Json.obj("term" -> Json.obj(field -> value))

}
