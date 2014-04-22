package fly.play.elasticsearch.query

import play.api.libs.json.{JsError, JsObject, Json, JsString, JsSuccess, JsValue, Reads, Writes}
import fly.play.elasticsearch.{EnumUtils, JsonUtils}
import scala.annotation.implicitNotFound

/**
 * Sorting of search-results.
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-request-sort.html
 * Ihe default sort order is probably desc, which makes sense for _score, but not for other fields.
 * Therefore, no default is given for the order.
 */
case class Sort(
  field: String,
  order: SortOrder.Value,
  mode: SortMode.Value = SortMode.default,
  missing: SortMissing.Value = SortMissing.default
)

object Sort extends JsonUtils {

  implicit val writes = new Writes[Sort] {
    def writes(sort: Sort): JsValue = Json.obj(
        sort.field -> toJsonObject(
          "order" -> JsString(sort.order.toString),
          "mode" -> toJsonIfNot(sort.mode.toString, SortMode.default.toString),
          "missing" -> toJsonIfNot("_"+sort.missing.toString, "_"+SortMissing.default.toString)
        )
      )
  }

  implicit val reads = new Reads[Sort] {
    def reads(json: JsValue) = json match {
      case JsObject(Seq((field, spec: JsObject))) =>
        JsSuccess(Sort(
            field,
            order = (spec \ "order").asOpt[SortOrder.Value].getOrElse(SortOrder.default),
            mode = (spec \ "mode").asOpt[SortMode.Value].getOrElse(SortMode.default),
            missing = (spec \ "missing").asOpt[SortMissing.Value].getOrElse(SortMissing.default)
          ))
      case badSort => JsError("Bad sort value: " + badSort.toString)
    }
  }

}

object SortOrder extends Enumeration {
  val asc, desc = Value
  val default = desc
  implicit val enumReads: Reads[Value] = EnumUtils.enumReads(SortOrder)
  implicit val enumWrites: Writes[Value] = EnumUtils.enumWrites
}

object SortMode extends Enumeration {
  val none, min, max, sum, avg = Value
  val default = none
  implicit val enumReads: Reads[Value] = EnumUtils.enumReads(SortMode)
  implicit val enumWrites: Writes[Value] = EnumUtils.enumWrites
}

object SortMissing extends Enumeration {
  val default, first, last = Value
  implicit val enumReads: Reads[Value] = EnumUtils.enumReadsWithTransform(SortMissing, {s => s.replaceFirst("^_", "")})
  implicit val enumWrites: Writes[Value] = EnumUtils.enumWritesWithTransform({s => "_"+s})
}

