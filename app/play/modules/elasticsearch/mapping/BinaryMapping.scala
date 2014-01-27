package play.modules.elasticsearch.mapping

import play.api.libs.json._
import play.modules.elasticsearch.JsonUtils

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/mapping-core-types.html#binary
 */
case class BinaryMapping(
    field: String
  ) extends NestableMapping with JsonUtils {

  def toJson: JsObject = Json.obj(field -> toJsonObject(
    "type" -> JsString(BinaryMapping.typeName)
  ))

}

object BinaryMapping extends MappingType {

  val typeName = "binary"

  val defaultFormat = "dateOptionalTime"

  override def fromJson(field: (String, JsValue)) = field match {case (fieldName, jsMapping) =>
    BinaryMapping(
      fieldName
    )
  }

}
