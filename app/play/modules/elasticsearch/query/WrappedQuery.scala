package play.modules.elasticsearch.query

import play.api.libs.json._

case class WrappedQuery(query: QueryImplementation, properties: Seq[(String, JsValue)] = Seq.empty) extends AbstractQuery {
  
  def addProperty[T](property: (String, T))(implicit writer:Writes[T]) = {
    val (key, value) = property
    copy(properties = properties :+ (key -> writer.writes(value)))
  }
    
  def toQueryDSL = query.toQueryDSL
  
  override def toJson: JsObject =
    super.toJson ++ JsObject(properties)
}
