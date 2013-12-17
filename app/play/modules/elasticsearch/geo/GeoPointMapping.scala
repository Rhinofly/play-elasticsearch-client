package play.modules.elasticsearch.geo

import play.modules.elasticsearch.MappingExtension
import play.modules.elasticsearch.JsonUtils

/**
 * Additional mapping options for geo_point.
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/mapping-geo-point-type.html#_mapping_options
 */
case class GeoPointMapping(
  indexLatLon: Boolean = false,
  indexGeohash: Boolean = false,
  geohashPrecision: GeoHashPrecisionType.Value = GeoHashPrecisionType.default,
  geohashPrefix: Boolean = false,
  validate: Boolean = false,
  validateLat: Boolean = false,
  validateLon: Boolean = false,
  normalize: Boolean = true,
  normalizeLat: Boolean = true,
  normalizeLon: Boolean = true
) extends MappingExtension with JsonUtils {

  def mappingJson =
    toJsonObject(
      "lat_lon" -> toJsonIfNot(indexLatLon, false),
      "geohash" -> toJsonIfNot(indexGeohash, false),
      "geohash_precision" -> toJsonIfNot(geohashPrecision, GeoHashPrecisionType.default),
      "geohash_prefix" -> toJsonIfNot(geohashPrefix, false),
      "validate" -> toJsonIfNot(validate, false),
      "validate_lat" -> toJsonIfNot(validateLat, false),
      "validate_lon" -> toJsonIfNot(validateLat, false),
      "normalize" -> toJsonIfNot(normalize, true),
      "normalize_lat" -> toJsonIfNot(normalizeLat, true),
      "normalize_lon" -> toJsonIfNot(normalizeLon, true)
    )

}

object GeoHashPrecisionType {
  type Value = String
  val default = "12"
}

