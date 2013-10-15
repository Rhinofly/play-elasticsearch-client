package play.modules.elasticsearch.query

import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.modules.elasticsearch.Parameter

abstract class Query {
  
//  def withProperties(properties: (String, AnyVal)*) : WrappedQuery = this match {
//    case query: WrappedQuery => query.withProperties(properties: _*)
//    case _ => new WrappedQuery(this).withProperties(properties: _*)
//  }

  def toQueryDSL: JsValue
   
  def toJson: JsValue = {
    Json.obj(
      "query" -> toQueryDSL
    )
  }
   
}

object Query {
  
  implicit def wrap(q: Query): WrappedQuery =
    new WrappedQuery(q)
  
}
