package fly.play.elasticsearch.geo

import play.api.libs.json.{JsObject, JsString, JsValue, Json}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import fly.play.elasticsearch.utils.JsonUtils
import fly.play.elasticsearch.mapping.{MappingType, NestableMapping}

/**
 * Additional mapping options for geo_point.
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/mapping-geo-point-type.html#_mapping_options
 */
case class GeoPointMapping(
    field: String,
    indexLatLon: Boolean = GeoPointMapping.defaultIndexLatLon,
    indexGeohash: Boolean = GeoPointMapping.defaultIndexGeohash,
    geohashPrecision: GeoHashPrecisionType.Value = GeoHashPrecisionType.default,
    geohashPrefix: Boolean = GeoPointMapping.defaultGeohashPrefix,
    validate: Boolean = GeoPointMapping.defaultValidate,
    validateLat: Boolean = GeoPointMapping.defaultValidateLat,
    validateLon: Boolean = GeoPointMapping.defaultValidateLon,
    normalize: Boolean = GeoPointMapping.defaultNormalize,
    normalizeLat: Boolean = GeoPointMapping.defaultNormalizeLat,
    normalizeLon: Boolean = GeoPointMapping.defaultNormalizeLon
  ) extends NestableMapping with JsonUtils {

  def toJson: JsObject = Json.obj(field -> toJsonObject(
    "type" -> JsString(GeoPointMapping.typeName),
    "lat_lon" -> toJsonIfNot(indexLatLon, GeoPointMapping.defaultIndexLatLon),
    "geohash" -> toJsonIfNot(indexGeohash, GeoPointMapping.defaultIndexGeohash),
    "geohash_precision" -> toJsonIfNot(geohashPrecision, GeoHashPrecisionType.default),
    "geohash_prefix" -> toJsonIfNot(geohashPrefix, GeoPointMapping.defaultGeohashPrefix),
    "validate" -> toJsonIfNot(validate, GeoPointMapping.defaultValidate),
    "validate_lat" -> toJsonIfNot(validateLat, GeoPointMapping.defaultValidateLat),
    "validate_lon" -> toJsonIfNot(validateLat, GeoPointMapping.defaultValidateLon),
    "normalize" -> toJsonIfNot(normalize, GeoPointMapping.defaultNormalize),
    "normalize_lat" -> toJsonIfNot(normalizeLat, GeoPointMapping.defaultNormalizeLat),
    "normalize_lon" -> toJsonIfNot(normalizeLon, GeoPointMapping.defaultNormalizeLon)
  ))

}

object GeoPointMapping extends MappingType {

  val typeName = "geo_point"

  val defaultIndexLatLon: Boolean = false
  val defaultIndexGeohash: Boolean = false
  val defaultGeohashPrefix: Boolean = false
  val defaultValidate: Boolean = false
  val defaultValidateLat: Boolean = false
  val defaultValidateLon: Boolean = false
  val defaultNormalize: Boolean = true
  val defaultNormalizeLat: Boolean = true
  val defaultNormalizeLon: Boolean = true

  override def fromJson(field: (String, JsValue)) = field match {case (fieldName, jsMapping) =>
    GeoPointMapping(
      fieldName,
      indexLatLon = (jsMapping \ "lat_lon").asOpt[Boolean].getOrElse(GeoPointMapping.defaultIndexLatLon),
      indexGeohash = (jsMapping \ "geohash").asOpt[Boolean].getOrElse(GeoPointMapping.defaultIndexGeohash),
      geohashPrecision = (jsMapping \ "geohash_precision").asOpt[GeoHashPrecisionType.Value].getOrElse(GeoHashPrecisionType.default),
      geohashPrefix = (jsMapping \ "geohash_prefix").asOpt[Boolean].getOrElse(GeoPointMapping.defaultGeohashPrefix),
      validate = (jsMapping \ "validate").asOpt[Boolean].getOrElse(GeoPointMapping.defaultValidate),
      validateLat = (jsMapping \ "validate_lat").asOpt[Boolean].getOrElse(GeoPointMapping.defaultValidateLat),
      validateLon = (jsMapping \ "validate_lon").asOpt[Boolean].getOrElse(GeoPointMapping.defaultValidateLon),
      normalize = (jsMapping \ "normalize").asOpt[Boolean].getOrElse(GeoPointMapping.defaultNormalize),
      normalizeLat = (jsMapping \ "normalize_lat").asOpt[Boolean].getOrElse(GeoPointMapping.defaultNormalizeLat),
      normalizeLon = (jsMapping \ "normalize_lon").asOpt[Boolean].getOrElse(GeoPointMapping.defaultNormalizeLon)
    )
  }

}


object GeoHashPrecisionType {
  type Value = String
  val default = "12"
}

