package play.modules.elasticsearch.query

import play.api.libs.json.JsValue
import play.api.libs.json.Json

abstract class Query {
  
   def toQueryDSL: JsValue
   
   def toJson: JsValue = {
     Json.obj(
       "query" -> toQueryDSL
     )
   }
   
}