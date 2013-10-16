package play.modules.elasticsearch.query

import play.api.libs.json._

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

}
