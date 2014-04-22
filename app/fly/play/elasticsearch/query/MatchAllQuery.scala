package fly.play.elasticsearch.query

import play.api.libs.json.JsValue
import play.api.libs.json._

case class MatchAllQuery() extends Query {
  
  def toQueryDSL =
    Json.obj( "match_all" -> 
      JsObject(Seq())
    )

}