package play.modules.elasticsearch.query

import play.api.libs.json.JsValue
import play.api.libs.json._

case class MatchAllQuery() extends AbstractQuery {
  
  def toQueryDSL =
    Json.obj( "match_all" -> 
      JsObject(Seq())
    )

}