package fly.play.elasticsearch.geo

import play.api.libs.json.{JsString, Json}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import fly.play.elasticsearch.utils.JsonUtils
import fly.play.elasticsearch.filter.Filter

/*
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-geo-distance-filter.html
 */
case class GeoDistanceFilter(
    field: String,
    location: GeoPoint,
    distance: Double,
    distanceUnit: DistanceUnit.Value = DistanceUnit.kilometers,
    distanceType: DistanceType.Value = DistanceType.default,
    optimizeBbox: OptimizeBbox.Value = OptimizeBbox.default
) extends Filter with JsonUtils {

  def toQueryDSL =
    Json.obj(
      "geo_distance" -> toJsonObject(
        field -> location.toJson,
        "distance" -> JsString(distance.toString+distanceUnit.toString()),
        "distance_type" -> toJsonIfNot(distanceType.toString, DistanceType.default.toString),
        "optimize_bbox" -> toJsonIfNot(optimizeBbox.toString, OptimizeBbox.default.toString)
      )
    )

}

object DistanceUnit extends Enumeration {
  val kilometers, meters, centimeters, millimeters, miles, yards, inch = Value
  // ES has no default distance unit, so we do not define it.
}

object DistanceType extends Enumeration {
  val arc, plane = Value
  def default = arc
}

object OptimizeBbox extends Enumeration {
  val memory, indexed, none = Value
  def default = memory
}

