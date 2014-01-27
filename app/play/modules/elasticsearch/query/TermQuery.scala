package play.modules.elasticsearch.query

import play.api.libs.json.Json

/*
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-term-query.html
 * Matches documents that have fields that contain a term. The term is not analyzed!
 */
case class TermQuery(
    field: String,
    value: String,
    boost: Double = -1.0
) extends Query {

  def toQueryDSL =
    if (boost < 0.0)
      Json.obj("term" -> Json.obj(field -> value))
    else
      Json.obj("term" -> Json.obj(field -> Json.obj("value" -> value, "boost" -> boost)))

}
