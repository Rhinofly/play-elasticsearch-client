package fly.play.elasticsearch.query

import play.api.libs.json.{JsValue, Reads, Writes}
import fly.play.elasticsearch.EnumUtils
import scala.language.implicitConversions

/**
 * Query using ElasticSearch Query-DSL as described in http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl.html.
 * This has different kinds of queries as sub-classes, see http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-queries.html.
 * As a general rule, queries should be used instead of filters:
 * * for full text search
 * * where the result depends on a relevance score
 */

trait Query {

  /**
   * Create a partial query that can be used in a compound query.
   */
  def toQueryDSL: JsValue

}

object Operator extends Enumeration {
  val or, and = Value
  implicit val enumReads: Reads[Value] = EnumUtils.enumReads(Operator)
  implicit val enumWrites: Writes[Value] = EnumUtils.enumWrites
}

object Query {
  /**
   * The Elasticsearch client does not use Query directly. It uses ElasticSearchQuery, which contains additional search properties.
   * This implicit function makes it possible to pass a Query to Client.search.
   */
  implicit def queryToElasticSearchQuery(query : Query): ElasticSearchQuery =
    ElasticSearchQuery(query)
}
