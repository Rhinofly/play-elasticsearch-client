package play.modules.elasticsearch

import play.api.libs.json.JsObject

/**
 * A mapping can have extensions, which specify additional mapping options in a controlled and type-safe way.
 * An example would be the geo_point type, which can have a lot of additional mapping options.
 *
 * Mapping extensions are written to ElasticSearch using the Writes[Mappings], but will not be read,
 * because there is no way to tell to which extension a mapping option belongs.
 */
trait MappingExtension {
  def mappingJson: JsObject
}
