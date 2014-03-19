package play.modules.elasticsearch.query

import play.api.libs.json.{JsArray, JsObject, JsValue, Json}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.modules.elasticsearch.JsonUtils

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-bool-query.html.
 * For the minimumShouldMatch parameter, see http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-minimum-should-match.html
 */
case class BoolQuery(
  shoulds: Seq[Query] = Seq.empty,
  musts: Seq[Query] = Seq.empty,
  mustNots: Seq[Query] = Seq.empty,
  minimumShouldMatch: String = ""
) extends Query with JsonUtils {

  def should(query: Query) =
    copy(shoulds = shoulds :+ query)

  def must(query: Query) =
    copy(musts = musts :+ query)

  def mustNot(query: Query) =
    copy(mustNots = mustNots :+ query)

  def toQueryDSL =
    Json.obj("bool" ->
      toJsonObject(
        "should" -> toJsonIfValid(shoulds, {queries: Seq[Query] => !queries.isEmpty}, {queries: Seq[Query] => queries.map(_.toQueryDSL)}),
        "must" -> toJsonIfValid(musts, {queries: Seq[Query] => !queries.isEmpty}, {queries: Seq[Query] => queries.map(_.toQueryDSL)}),
        "must_not" -> toJsonIfValid(mustNots, {queries: Seq[Query] => !queries.isEmpty}, {queries: Seq[Query] => queries.map(_.toQueryDSL)}),
        "minimum_should_match" -> toJsonIfValid[String](minimumShouldMatch, { _ != "" })
      ))

}
