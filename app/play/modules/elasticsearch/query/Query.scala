package play.modules.elasticsearch.query

import play.api.libs.json._

/* Query using ElasticSearch Query-DSL as described in http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl.html.
 * This has different kinds of queries as sub-classes, see http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-queries.html.
 */

abstract class Query {
  
  def toQueryDSL: JsValue
   
  def toJson: JsValue = {
    Json.obj(
      "query" -> toQueryDSL
    )
  }

  def wrapped : WrappedQuery = this match {
    case wrapped: WrappedQuery => wrapped
    case otherQuery => new WrappedQuery(otherQuery)
  }
  
  def withVersion(version: Boolean) =
    wrapped.addProperty(("version" -> JsBoolean(version)))

  def withFrom(from: Int) =
    wrapped.addProperty(("from" -> JsNumber(from)))

  def withSize(size: Int) =
    wrapped.addProperty(("size" -> JsNumber(size)))

  /* Sub-classes use toJsonIfValid and isValidJsonProperty to construct query-property lists. */
    
  protected def toJsonIfValid[T: Writes](value : T, isValid: T => Boolean) : JsValue =
    if (isValid(value)) Json.toJson(value) else JsNull
    
  protected def isValidJsonProperty(property: (String, JsValue)) = property match {
    case (k, v) => (v != JsNull)
  }
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
