package play.modules.elasticsearch.query

import play.api.libs.json._
import scala.language.implicitConversions

trait Query {

  /**
   * Create a partial query that can be used in a compound query.
   */
  def toQueryDSL: JsValue
  
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

object Query {
  implicit def queryToElasticSearchQuery(query : Query): ElasticSearchQuery =
    ElasticSearchQuery(query)
}
