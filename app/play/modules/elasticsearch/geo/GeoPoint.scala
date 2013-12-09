package play.modules.elasticsearch.geo

import play.api.libs.json.{Json, JsValue, JsObject, JsArray, JsNumber, JsString, Writes, Reads, JsSuccess}
import scala.language.implicitConversions
import play.api.libs.json.JsError

/**
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/mapping-geo-point-type.html
 * Applications can use this type to create geo_point data that is indexable for ES.
 */
trait GeoPoint {
  def toJson : JsValue
}

object GeoPoint {

  implicit val writes = new Writes[GeoPoint] {
    def writes(point: GeoPoint): JsValue = point.toJson
  }

  implicit val reads = new Reads[GeoPoint] {
    def reads(json: JsValue) = json match {
      case JsObject(Seq(("lat", JsNumber(lat)), ("lon", JsNumber(lon)))) =>
        JsSuccess(GeoLatLon(lat.toDouble, lon.toDouble))
      case JsArray(Seq(JsNumber(lon), JsNumber(lat))) =>
        JsSuccess(GeoLatLon(lat.toDouble, lon.toDouble))
      case JsString(value) if value.matches("\\d+(\\.\\d+)?,\\d+(\\.\\d+)?") =>
        JsSuccess(GeoLatLon(value))
      case JsString(value) => JsSuccess(GeoHash(value))
      case _ => JsError("Bad geo_point value: "+json.toString)
    }
  }

}


case class GeoLatLon(lat: Double, lon: Double) extends GeoPoint {
  def toJson =
    Json.obj(
      "lat" -> Json.toJson(lat),
      "lon" -> Json.toJson(lon)
    )
}

object GeoLatLon {
  def apply(latLon: String) = {
    val Array(lat, lon) = latLon.split("\\s*,\\s*").map{_.toDouble}
    new GeoLatLon(lat = lat, lon = lon)
  }
}


/* See geohash.org. */
case class GeoHash(hash: String) extends GeoPoint {
  def toJson =
    JsString(hash)
}
