package play.modules.elasticsearch.query

import play.api.libs.json._

case class WrappedQuery(query: Query, properties: Map[String, JsValue] = Map()) extends Query {
  
  def addProperty(property: (String, JsValue)) =
    copy(properties = properties + property)
    
  def toQueryDSL = query.toQueryDSL
  
  override def toJson: JsValue = {
    JsObject(properties.toSeq ++ Seq("query" -> query.toQueryDSL))
  }
    
}
