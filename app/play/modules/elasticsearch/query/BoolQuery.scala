package play.modules.elasticsearch.query

import play.api.libs.json.{JsArray, JsObject, JsValue, Json}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.modules.elasticsearch.JsonUtils

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-bool-query.html.
 * For the minimumShouldMatch parameter, see http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-minimum-should-match.html
 */
case class BoolQuery(
  shoulds: List[Query] = List(),
  musts: List[Query] = List(),
  mustNots: List[Query] = List(),
  minimumShouldMatch: String = "") extends Query with JsonUtils {

  def should(query: Query) =
    copy(shoulds = shoulds :+ query)

  def must(query: Query) =
    copy(musts = musts :+ query)

  def mustNot(query: Query) =
    copy(mustNots = mustNots :+ query)

  def toQueryDSL =
    Json.obj("bool" ->
      JsObject(
        subQueries("should", shoulds) ++ subQueries("must", musts) ++ subQueries("must_not", mustNots) :+
        ("minimum_should_match" -> toJsonIfValid[String](minimumShouldMatch, { _ != "" }))
      ))

  private def subQueries(name: String, queries: List[Query]): Seq[(String, JsValue)] =
    queries match {
      case Nil => Nil
      case List(query) => Seq(name -> query.toQueryDSL)
      case queries => Seq(name -> JsArray(queries.map { query => query.toQueryDSL }))
    }

}