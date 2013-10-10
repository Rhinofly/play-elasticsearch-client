package play.modules.elasticsearch

import play.api.libs.json.JsValue

case class ElasticSearchException(status: Int, message: String, body:JsValue)
  extends RuntimeException(s"Status: $status, message: $message")