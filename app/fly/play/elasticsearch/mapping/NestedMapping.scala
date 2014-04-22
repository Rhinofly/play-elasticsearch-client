package fly.play.elasticsearch.mapping

import play.api.libs.json.{JsObject, JsString, JsValue, Json}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import fly.play.elasticsearch.{ElasticSearchException, JsonUtils}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/mapping-nested-type.html,
 *     http://www.elasticsearch.org/blog/managing-relations-inside-elasticsearch/
 */
case class NestedMapping(
    field: String,
    properties: Set[NestableMapping],
    dynamic: Boolean = NestedMapping.defaultDynamic,
    includeInParent: Boolean = NestedMapping.defaultIncudeInParent,
    includeInRoot: Boolean = NestedMapping.defaultIncludeInRoot
  ) extends NestableMapping with JsonUtils {

  override def toJson: JsObject = Json.obj(field -> toJsonObject(
      "type" -> JsString(NestedMapping.typeName),
      "properties" -> JsObject(properties.flatMap(_.toJson.fields).toSeq),
      "dynamic" -> toJsonIfNot(dynamic, NestedMapping.defaultDynamic),
      "include_in_parent" -> toJsonIfNot(includeInParent, NestedMapping.defaultIncudeInParent),
      "include_in_root" -> toJsonIfNot(includeInRoot, NestedMapping.defaultIncludeInRoot)
    ))

}

object NestedMapping extends MappingType {

  val typeName = "nested"

  val defaultDynamic = true
  val defaultIncudeInParent = false
  val defaultIncludeInRoot = false

  override def fromJson(field: (String, JsValue)) = field match {case (fieldName, jsMapping) =>
    NestedMapping(
      fieldName,
      properties = (jsMapping \ "properties") match {
        case JsObject(fields) => fields.map(Mapping.fromJson(_)).toSet
        case _ => throw ElasticSearchException(-1, "Bad properties in NestedMapping.fromJson: ", jsMapping)
      },
      dynamic = (jsMapping \ "dynamic").asOpt[Boolean].getOrElse(defaultDynamic),
      includeInParent = (jsMapping \ "include_in_parent").asOpt[Boolean].getOrElse(defaultIncudeInParent),
      includeInRoot = (jsMapping \ "include_in_root").asOpt[Boolean].getOrElse(defaultIncludeInRoot)
    )
  }

}
