package play.modules.elasticsearch.geo

import play.api.libs.json.Json
import play.modules.elasticsearch.filter.Filter

/*
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-geo-bounding-box-filter.html
 */
case class GeoBoundingBoxFilter(field: String, topLeft: GeoPoint, bottomRight: GeoPoint) extends Filter {

  def toQueryDSL =
    Json.obj(
      "geo_bounding_box" -> Json.obj(
        field -> Json.obj(
          "top_left" -> topLeft.toJson,
          "bottom_right" -> bottomRight.toJson
        )
      )
    )

}
