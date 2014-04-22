package fly.play.elasticsearch.query

import play.api.libs.json._
import fly.play.elasticsearch.JsonUtils
import fly.play.elasticsearch.filter.Filter

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-filtered-query.html
 */
case class FilteredQuery(
    query: Query = noQuery,
    filter: Filter
) extends Query with JsonUtils {

  def toQueryDSL =
    Json.obj("filtered" ->
      toJsonObject(
        "query" -> query.toQueryDSL,
        "filter" -> filter.toQueryDSL
      )
    )

}

/* Use a special Query object instead of Option[Query], to avoid having to use Some(query) when making a FilteredQuery. */
private object noQuery extends Query {
  def toQueryDSL = JsNull
}
