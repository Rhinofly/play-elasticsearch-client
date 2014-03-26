package play.modules.elasticsearch.query

import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper

/*
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-boosting-query.html
 */
case class BoostingQuery(
  positive: Query,
  negative: Query,
  negativeBoost: Double
) extends Query {

  def toQueryDSL =
    Json.obj("boosting" ->
      Json.obj(
        "positive" -> positive.toQueryDSL,
        "negative" -> negative.toQueryDSL,
        "negative_boost" -> negativeBoost
      ))

}