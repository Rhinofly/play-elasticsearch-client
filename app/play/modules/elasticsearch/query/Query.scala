package play.modules.elasticsearch.query

import play.api.libs.json._

/* Query using ElasticSearch Query-DSL as described in http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl.html.
 * This has different kinds of queries as sub-classes, see http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-queries.html.
 */

trait Query {
  def toJson: JsObject
}

trait QueryImplementation extends Query {
  def toJson: JsObject =
    Json.obj("query" -> toQueryDSL)

  def toQueryDSL: JsValue
}

abstract class AbstractQuery extends QueryImplementation {

  def wrapped: WrappedQuery =
    this match {
      case wrapped: WrappedQuery => wrapped
      case otherQuery => new WrappedQuery(otherQuery)
    }

  def withVersion(version: Boolean) =
    wrapped.addProperty("version" -> version)

  def withFrom(from: Int) =
    wrapped.addProperty("from" -> from)

  def withSize(size: Int) =
    wrapped.addProperty("size" -> size)

}

object Operator extends Enumeration {
  val or = Value("or")
  val and = Value("and")
}

object MatchType extends Enumeration {
  val boolean = Value("boolean")
  val phrase = Value("phrase")
  val phrase_prefix = Value("phrase_prefix")
}
