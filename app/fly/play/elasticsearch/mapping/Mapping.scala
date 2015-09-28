package fly.play.elasticsearch.mapping

import fly.play.elasticsearch.ElasticSearchException
import fly.play.elasticsearch.geo.GeoPointMapping
import fly.play.elasticsearch.utils.EnumUtils
import play.api.libs.json.{JsObject, JsSuccess, JsValue, Json}
import play.api.libs.json.{Reads, Writes}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import scala.annotation.implicitNotFound

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/mapping.html
 * A Mapping can be specified on an index or a type, through the appropriate methods in Client.
 */
trait Mapping {

  /**
   * The name of the field that uses the mapping.
   */
  def field: String

  /**
   * Make a JSON representation. Must return a JsObject("field" -> JsObject(...)).
   */
  def toJson: JsObject

  def withFieldData(fieldData: FieldData): Mapping =
    new MappingWithFieldData(this, fieldData)

}

/**
 * A NestableMapping can be part of an ObjectMapping.
 * Most sub-classes of Mapping are nestable, but RootObjectMapping is not!
 */
trait NestableMapping extends Mapping

/**
 * The type of a companion object for a sub-class of Mapping.
 */
trait MappingType {

  /**
   * The name for this type.
   */
  val typeName: String

  /**
   * Make a Mapping from JSON. Takes a ("fieldName" -> JsObject(...)) tuple.
   */
  def fromJson(field: (String, JsValue)): NestableMapping

}

object Mapping {

  /**
   * Make several type mappings into one mappings definition, used at index creation.
   * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/indices-create-index.html#mappings.
   */
  def jsonForMappings(mappings: Seq[Mapping]) = Json.obj("mappings" -> JsObject(
      mappings map {_.toJson} collect {case obj: JsObject => obj.fields(0)}
    ))

  implicit val writes: Writes[Mapping] = Writes {
    mapping => mapping.toJson
  }

  /**
   * Read a mapping from JSON by selecting the appropriate fromJson function.
   * @param field A tuple of a field-name and the mapping definition for that field.
   */
  def fromJson(field: (String, JsValue)): NestableMapping = {
   field match {case (_, fieldSpec) =>
      // The default type-name is "object", because "if there are properties defined for it, it will automatically be identified as an object mapping."
      val typeName = (fieldSpec \ "type").asOpt[String].getOrElse("object")
      typeName match {
        case ObjectMapping.typeName => ObjectMapping.fromJson(field)
        case StringMapping.typeName => StringMapping.fromJson(field)
        case NumberMapping.float.typeName => NumberMapping.float.fromJson(field)
        case NumberMapping.double.typeName => NumberMapping.double.fromJson(field)
        case NumberMapping.integer.typeName => NumberMapping.integer.fromJson(field)
        case NumberMapping.long.typeName => NumberMapping.long.fromJson(field)
        case NumberMapping.short.typeName => NumberMapping.short.fromJson(field)
        case NumberMapping.byte.typeName => NumberMapping.byte.fromJson(field)
        case TokenCountMapping.typeName => TokenCountMapping.fromJson(field)
        case DateMapping.typeName => DateMapping.fromJson(field)
        case BooleanMapping.typeName => BooleanMapping.fromJson(field)
        case BinaryMapping.typeName => BinaryMapping.fromJson(field)
        case MultiFieldMapping.typeName => MultiFieldMapping.fromJson(field)
        case GeoPointMapping.typeName => GeoPointMapping.fromJson(field)
        case NestedMapping.typeName => NestedMapping.fromJson(field)
      }
    }
  }

  /**
   * Read a top-level mapping from JSON.
   * This starts with the NestableMappiung properties, then adds extra properties for field data and root object.
   * @param field A JSON structure of the form
   *   { <type> : { "properties" : ... } }
   */
  def fromJsonRoot(field: (String, JsValue)): Mapping =
    field match {
      case (_, fieldSpec) => RootObjectMapping.addToFromJson(fieldSpec, MappingWithFieldData.addToFromJson(fieldSpec, fromJson(field)))
      case _ => throw ElasticSearchException(-1, "Bad mappings received in fromJsonRoot("+field._1+").", field._2)
    }

  /**
   * Read mappings for an index.
   * This must be applied to the JSON from a response to "<index>/_mapping" or "<index>/<type>/_mapping".
   * The JSON for the mappings is like:
   *   { <index> : { "mappings" : { <type> : { "properties" : ... } } } }
   */
  def mappingsFromJson(mappings: JsValue): Seq[Mapping] = mappings match {
    case JsObject(Seq( (indexName, JsObject(Seq( ("mappings", jsMappings)))))) => jsMappings match {
      case JsObject(fields) => fields map TypeMapping.fromJson
      case _ => Seq.empty[Mapping]
    }
    case _ => throw ElasticSearchException(-1, "Bad mappings received in mappingsFromJson.", mappings)
  }

  /**
   * The reads expects JSON of the form
   *   { <index> : { "mappings" : { <type> : { "properties" :  ... } } } }
   */
  implicit val reads: Reads[Mapping] = Reads {
    case json: JsObject => JsSuccess(mappingsFromJson(json).head)
    case mappings => throw ElasticSearchException(-1, "Bad mappings received in reads.", mappings)
  }

}


/**
 * Types for common mapping options.
 * Some of these are Enumerations. The others we make look like they are enumerations by defining a type Value.
 */

/**
 * The documentation [http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/mapping-core-types.html] says:
 * Set to yes to store actual field in the index, no to not store it. Defaults to no (note, the JSON document itself is stored, and it can be retrieved from it).
 * But in the mapping returned by ES, this is a boolean (true / false).
 */
object StoreType {
  type Value = Boolean
  val yes = true
  val no = false
  val default = false
}

object IndexType extends Enumeration {
  val analyzed, no, not_analyzed = Value
  def default = analyzed
  implicit val enumReads: Reads[Value] = EnumUtils.enumReads(IndexType)
  implicit val enumWrites: Writes[Value] = EnumUtils.enumWrites
}

object TermVectorType extends Enumeration {
  val no, yes, with_offsets, with_positions, with_positions_offsets = Value
  def default = no
  implicit val enumReads: Reads[Value] = EnumUtils.enumReads(TermVectorType)
  implicit val enumWrites: Writes[Value] = EnumUtils.enumWrites
}

object BoostType {
  type Value = Double
  val default = 1.0
}

object AnalyzerType {
  type Value = String
  val default = "default"
}
