package fly.play.elasticsearch.mapping

import fly.play.elasticsearch.ElasticSearchException
import fly.play.elasticsearch.utils.JsonUtils
import play.api.libs.json._

case class TypeMapping(
  field: String,
  properties: Set[NestableMapping]
) extends NestableMapping with JsonUtils {

  def toJson: JsObject = Json.obj(
    field -> Json.obj(
      "properties" -> JsObject(properties.flatMap(_.toJson.fields).toSeq)
    )
  )
}

object TypeMapping {

  def fromJson(field: (String, JsValue)) = field match {case (fieldName, jsMapping) =>
    TypeMapping(
      fieldName,
      properties = (jsMapping \ "properties") match {
        case JsObject(fields) => fields.map(Mapping.fromJson(_)).toSet
        case _ => throw ElasticSearchException(-1, "Bad properties in ObjectMapping.fromJson: ", jsMapping)
      }
    )
  }
}