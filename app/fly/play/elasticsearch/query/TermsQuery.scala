package fly.play.elasticsearch.query

import play.api.libs.json._

/*
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-terms-query.html
 */
case class TermsQuery(field: String, values: Seq[String], minimum_match: Int = 1) extends Query {
  
  def toQueryDSL =
    Json.obj("terms" -> Json.obj(field -> Json.toJson(values), "minimum_match" -> JsNumber(minimum_match)))

}
