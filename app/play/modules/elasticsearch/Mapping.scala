package play.modules.elasticsearch

import play.api.libs.json._
import play.api.data.validation.ValidationError

/**
 * Mappings for ES indexes and types.
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/mapping.html
 *     http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/mapping-core-types.html
 *
 * The Mapping class defines mappings for field types.
 * The name of the field at the top-level is the type that is mapped in ElasticSearch.
 *
 * Every item in a mapping can have a default value. Default values are not included when specifying a mapping to ES.
 * In this way, items that are not applicable for a specific mapping-type will be absent from the mapping (if they have the default values).
 *
 * The Mapping class only defines the core mapping options.
 * Additional type-specific options are defined in extensions, see the MappingExtension trait.
 */

case class Mapping(
    field: String,
    fieldType: MappingType.Value = MappingType.default,
    index: IndexType.Value = IndexType.default,
    store: StoreType.Value = StoreType.default,
    boost: BoostType.Value = BoostType.default,
    termVector: TermVectorType.Value = TermVectorType.default,
    analyzer: AnalyzerType.Value = AnalyzerType.default,
    indexAnalyzer: AnalyzerType.Value = AnalyzerType.default,
    searchAnalyzer: AnalyzerType.Value = AnalyzerType.default,
    properties: Seq[Mapping] = Seq.empty,
    extensions: Seq[MappingExtension] = Seq.empty
  ) {

  def extendWith(extension: MappingExtension) =
    copy(extensions = extension +: extensions)

}

/* Types for mapping options. */

object MappingType extends Enumeration {
  val `object`, string, byte, short, integer, long, float, double, date, boolean, binary, geo_point = Value
  val default = `object`
  implicit val enumReads: Reads[Value] = EnumUtils.enumReads(MappingType)
  implicit def enumWrites: Writes[Value] = EnumUtils.enumWrites
}

object IndexType extends Enumeration {
  val analyzed, no, not_analyzed = Value
  val default = analyzed
  implicit val enumReads: Reads[Value] = EnumUtils.enumReads(IndexType)
  implicit def enumWrites: Writes[Value] = EnumUtils.enumWrites
}

/*
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

object BoostType {
  type Value = Double
  val default = 1.0
}

object TermVectorType extends Enumeration {
  val no, yes, with_offsets, with_positions, with_positions_offsets = Value
  val default = no
  implicit val enumReads: Reads[Value] = EnumUtils.enumReads(TermVectorType)
  implicit def enumWrites: Writes[Value] = EnumUtils.enumWrites
}

object AnalyzerType {
  type Value = String
  val default = "default"
}

/* Companion object for Reads and Writes. */

object Mapping extends JsonUtils {

  // Writes mappings.

  implicit object mappingWrites extends Writes[Mapping] {
    def writes(mapping: Mapping) =
      Json.obj( mapping.field -> mappingJson(mapping) )
  }

  /* Convert mapping for a field into JSON. */
  def mappingJson(mapping: Mapping): JsValue = {
    val baseMapping = toJsonObject(
        "type" -> toJsonIfNot(mapping.fieldType, MappingType.default),
        "index" -> toJsonIfNot(mapping.index, IndexType.default),
        "store" -> toJsonIfNot(mapping.store, StoreType.default),
        "boost" -> toJsonIfNot(mapping.boost, BoostType.default),
        "term_vector" -> toJsonIfNot(mapping.termVector, TermVectorType.default),
        "analyzer" -> toJsonIfNot(mapping.analyzer, AnalyzerType.default),
        "index_analyzer" -> toJsonIfNot(mapping.indexAnalyzer, AnalyzerType.default),
        "search_analyzer" -> toJsonIfNot(mapping.searchAnalyzer, AnalyzerType.default),
        "properties" -> toJsonObject(mapping.properties.map{jsonTuple}:_*)
      )
    /* Extensions have their own mappingJson, so the following is not a recursion. */
    val extensions = mapping.extensions.map(_.mappingJson)
    /* Concatenate the base mapping and all extensions. */
    (baseMapping /: extensions) (_ ++_)
  }

  /* Tuple for field and mapping. */
  def jsonTuple(mapping: Mapping) = (mapping.field -> mappingJson(mapping))

  /**
   * Make several type mappings into one mappings definition, used at index creation.
   * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/indices-create-index.html#mappings.
   */
  def jsonForTypes(mappings: Seq[Mapping]) = Json.obj("mappings" -> JsObject(mappings.map{jsonTuple}))

  // Reads mappings.

  implicit object mappingReads extends Reads[Mapping] {
    def reads(json: JsValue): JsResult[Mapping] = json match {
      case JsObject(Seq((field, jsMapping))) => JsSuccess(jsonMapping(field, jsMapping))
      case _ => JsError(ValidationError("A mapping must be a JSON object, which maps a type to its mapping."))
    }
  }

  def jsonMapping(field: String, jsMapping: JsValue): Mapping =
    Mapping(
      field,
      fieldType = (jsMapping \ "type").asOpt[MappingType.Value].getOrElse(MappingType.default),
      index = (jsMapping \ "index").asOpt[IndexType.Value].getOrElse(IndexType.default),
      store = (jsMapping \ "store").asOpt[StoreType.Value].getOrElse(StoreType.default),
      boost = (jsMapping \ "boost").asOpt[BoostType.Value].getOrElse(BoostType.default),
      termVector = (jsMapping \ "term_vector").asOpt[TermVectorType.Value].getOrElse(TermVectorType.default),
      analyzer = (jsMapping \ "analyzer").asOpt[AnalyzerType.Value].getOrElse(AnalyzerType.default),
      indexAnalyzer = (jsMapping \ "index_analyzer").asOpt[AnalyzerType.Value].getOrElse(AnalyzerType.default),
      searchAnalyzer = (jsMapping \ "search_analyzer").asOpt[AnalyzerType.Value].getOrElse(AnalyzerType.default),
      properties = propertiesMappings(jsMapping \ "properties")
    )

  def propertiesMappings(jsMappings: JsValue): Seq[Mapping] = jsMappings match {
    case JsObject(fields) => fields.map{case(field, mapping) => jsonMapping(field, mapping)}
    case _ => Seq.empty[Mapping]
  }

  /**
   * Read mappings from an index.
   * This must only be applied to the JSON from a response to "<index>/_mapping". Thus, the the top-level key is the index name, and is discarded.
   */
  def typesFromJson(mapping: JsValue): Seq[Mapping] = mapping match {
    case JsObject(Seq((_, jsMappings))) => propertiesMappings(jsMappings)
    case _ => throw ElasticSearchException(-1, "Bad mapping received.", mapping)
  }

}
