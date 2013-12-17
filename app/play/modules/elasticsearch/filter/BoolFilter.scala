package play.modules.elasticsearch.filter

import play.api.libs.json.{JsArray, JsObject, JsValue, Json}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.modules.elasticsearch.JsonUtils

/*
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-bool-filter.html
 * For the minimumShouldMatch parameter, see http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-minimum-should-match.html
 * The Bool filter does not support minimum_should_match, see https://github.com/elasticsearch/elasticsearch/issues/4142
 */
case class BoolFilter(
  shoulds: Seq[Filter] = Seq.empty,
  musts: Seq[Filter] = Seq.empty,
  mustNots: Seq[Filter] = Seq.empty
) extends Filter with JsonUtils {

  def should(filter: Filter) =
    copy(shoulds = shoulds :+ filter)

  def must(filter: Filter) =
    copy(musts = musts :+ filter)

  def mustNot(filter: Filter) =
    copy(mustNots = mustNots :+ filter)

  def toQueryDSL =
    Json.obj("bool" ->
      JsObject(
        subFilters("should", shoulds) ++ subFilters("must", musts) ++ subFilters("must_not", mustNots)
      ))

  private def subFilters(name: String, filters: Seq[Filter]): Seq[(String, JsValue)] =
    filters match {
      case Nil => Nil
      case List(filter) => Seq(name -> filter.toQueryDSL)
      case filters => Seq(name -> JsArray(filters.map { filter => filter.toQueryDSL }))
    }

}
