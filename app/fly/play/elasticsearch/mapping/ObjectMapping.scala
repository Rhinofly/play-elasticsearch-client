package fly.play.elasticsearch.mapping

import play.api.libs.json.{JsObject, JsString, JsValue, Json}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import fly.play.elasticsearch.{ElasticSearchException, JsonUtils}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/mapping-object-type.html
 */
/*TODO The path property is deprecated in 1.0.0. */
case class ObjectMapping(
    field: String,
    properties: Set[NestableMapping],
    dynamic: Boolean = ObjectMapping.defaultDynamic,
    enabled: Boolean = ObjectMapping.defaultEnabled,
    path: Option[String] = None
  ) extends NestableMapping with JsonUtils {

  /**
   * Make a JSON representation. Must return a JsObject("field" -> JsObject(...)).
   */
  override def toJson: JsObject = Json.obj(field -> toJsonObject(
      "type" -> JsString(ObjectMapping.typeName),
      "properties" -> JsObject(properties.flatMap(_.toJson.fields).toSeq),
      "dynamic" -> toJsonIfNot(dynamic, ObjectMapping.defaultDynamic),
      "enabled" -> toJsonIfNot(enabled, ObjectMapping.defaultEnabled),
      "path" -> toJsonIfNot(path, None)
    ))

}

object ObjectMapping extends MappingType {

  val typeName = "object"

  val defaultDynamic = true
  val defaultEnabled = true

  /**
   * Make a Mapping from JSON. Takes a ("fieldName" -> JsObject(...)) tuple.
   */
  override def fromJson(field: (String, JsValue)) = field match {case (fieldName, jsMapping) =>
    ObjectMapping(
      fieldName,
      properties = (jsMapping \ "properties") match {
        case JsObject(fields) => fields.map(Mapping.fromJson(_)).toSet
        case _ => throw ElasticSearchException(-1, "Bad properties in ObjectMapping.fromJson: ", jsMapping)
      },
      dynamic = (jsMapping \ "dynamic").asOpt[Boolean].getOrElse(defaultDynamic),
      enabled = (jsMapping \ "enabled").asOpt[Boolean].getOrElse(defaultEnabled),
      path = (jsMapping \ "path").asOpt[String]
    )
  }

}
