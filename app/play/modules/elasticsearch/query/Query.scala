package play.modules.elasticsearch.query

import play.api.libs.json.{JsValue, Reads, Writes}
import play.modules.elasticsearch.EnumUtils
import scala.language.implicitConversions

/**
 * Query using ElasticSearch Query-DSL as described in http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl.html.
 * This has different kinds of queries as sub-classes, see http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-queries.html.
 */

trait Query {

  /**
   * Create a partial query that can be used in a compound query.
   */
  def toQueryDSL: JsValue

}

object Operator extends Enumeration {
  val or = Value("or")
  val and = Value("and")
  implicit val enumReads: Reads[Value] = EnumUtils.enumReads(Operator)
  implicit val enumWrites: Writes[Value] = EnumUtils.enumWrites
}

object MatchType extends Enumeration {
  val boolean = Value("boolean")
  val phrase = Value("phrase")
  val phrase_prefix = Value("phrase_prefix")
  implicit val enumReads: Reads[Value] = EnumUtils.enumReads(MatchType)
  implicit val enumWrites: Writes[Value] = EnumUtils.enumWrites
}

object Query {
  implicit def queryToElasticSearchQuery(query : Query): ElasticSearchQuery =
    ElasticSearchQuery(query)
}
