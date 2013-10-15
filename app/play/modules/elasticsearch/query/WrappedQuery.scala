package play.modules.elasticsearch.query

import play.api.libs.json._

case class WrappedQuery(wrapped: Query, properties: Map[String, JsValue] = Map()) extends Query {

  def withVersion(version: Boolean) =
    copy(properties = properties + ("version" -> JsBoolean(version)))
    
  def toQueryDSL = wrapped.toQueryDSL
  
  override def toJson: JsValue = {
    JsObject(properties.toSeq ++ Seq("query" -> wrapped.toQueryDSL))
  }
    
}
