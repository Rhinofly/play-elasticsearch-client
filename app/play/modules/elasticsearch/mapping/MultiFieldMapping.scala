package play.modules.elasticsearch.mapping

import play.api.libs.json.{JsObject, JsString, JsValue, Json}
import play.api.libs.json.{Reads, Writes}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.modules.elasticsearch.{ElasticSearchException, EnumUtils, JsonUtils}

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/mapping-multi-field-type.html
 */
case class MultiFieldMapping(
    field: String,
    fields: Seq[Mapping],
    path : MultiFieldMapping.PathType.Value = MultiFieldMapping.PathType.default
  ) extends NestableMapping with JsonUtils {

  override def toJson: JsObject = Json.obj(field -> toJsonObject(
      "type" -> JsString(MultiFieldMapping.typeName),
      "fields" -> (fields.map(_.toJson) :\ JsObject(Seq())) (_ ++ _),
      "path" -> toJsonIfNot(path, MultiFieldMapping.PathType.default)
    ))

}

object MultiFieldMapping extends MappingType {

  val typeName = "multi_field"

  override def fromJson(field: (String, JsValue)) = field match {case (fieldName, jsMapping) =>
    MultiFieldMapping(
      fieldName,
      fields = (jsMapping \ "fields") match {
        case fieldDefs: JsObject => fieldDefs.fields.map{case(name, json) => Mapping.fromJson((name, json))}
        case _ => throw ElasticSearchException(-1, "Bad fields object in multi_field mapping.", jsMapping)
      },
      path = (jsMapping \ "path").asOpt[MultiFieldMapping.PathType.Value].getOrElse(MultiFieldMapping.PathType.default)
    )
  }

  object PathType extends Enumeration {
    import play.modules.elasticsearch.EnumUtils
    val full, just_name = Value
    val default = full
    implicit val enumReads: Reads[Value] = EnumUtils.enumReads(PathType)
    implicit def enumWrites: Writes[Value] = EnumUtils.enumWrites
  }

}

