package play.modules.elasticsearch.filter

import play.api.libs.json.Json

/*
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-missing-filter.html
 */
case class MissingFilter(
  field: String,
  existence: Boolean = true,
  nullValue: Boolean = true
) extends Filter {

  def toQueryDSL =
    Json.obj("missing" -> Json.obj("field" -> field, "existence" -> existence, "null_value" -> nullValue))

}
