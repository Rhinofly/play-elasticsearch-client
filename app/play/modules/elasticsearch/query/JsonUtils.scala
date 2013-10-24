package play.modules.elasticsearch.query

import play.api.libs.json.Writes
import play.api.libs.json.JsValue
import play.api.libs.json.JsNull
import play.api.libs.json.Json

trait JsonUtils {
  
  protected def toJsonIfValid[T: Writes](value: T, isValid: T => Boolean): JsValue =
    if (isValid(value)) Json.toJson(value) else JsNull

  protected def isValidJsonProperty(property: (String, JsValue)) =
    property match {
      case (k, v) => (v != JsNull)
    }

}