package play.modules.elasticsearch.query

import play.api.libs.json.{JsArray, JsObject, JsValue, Json}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.modules.elasticsearch.JsonUtils

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-dis-max-query.html.
 */
case class DisMaxQuery(
  queries: Seq[Query] = Seq.empty,
  tieBreaker: Double = DisMaxQuery.defaultTieBreaker,
  boost: Double = DisMaxQuery.defaultBoost
) extends Query with JsonUtils {

  def union(query: Query) =
    copy(queries = queries :+ query)

  def toQueryDSL =
    Json.obj("dis_max" ->
      toJsonObject(
        "queries" -> JsArray(queries.map(_.toQueryDSL)),
        "tie_breaker" -> toJsonIfNot(tieBreaker, DisMaxQuery.defaultTieBreaker),
        "boost" -> toJsonIfNot(boost, DisMaxQuery.defaultBoost)
      ))

}

object DisMaxQuery {
  val defaultTieBreaker = 0.0
  val defaultBoost: Double = 1.0
}
