package fly.play.elasticsearch.filter

import play.api.libs.json.Json

/*
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-exists-filter.html
 */
case class ExistsFilter(field: String) extends Filter {

  def toQueryDSL =
    Json.obj("exists" -> Json.obj("field" -> field))

}
