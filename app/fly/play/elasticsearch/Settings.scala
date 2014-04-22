package fly.play.elasticsearch

import play.api.libs.json._
import play.api.libs.json.util._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import play.api.data.validation.ValidationError
import fly.play.elasticsearch.analysis._
import fly.play.elasticsearch.mapping.Mapping
import fly.play.elasticsearch.utils.JsonUtils

/**
 * Settings for ES indices.
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/indices-create-index.html
 */
case class Settings(
    nrOfShards: Int = 1,
    nrOfReplicas: Int = 0,
    analysis: Option[Analysis] = None
  ) {

  def toJson =
    Json.obj("settings" -> Settings.settingsFormat.writes(this))

  def toJsonWithMappings(mappings: Seq[Mapping]) =
    toJson ++ Mapping.jsonForMappings(mappings)

}

object Settings extends JsonUtils {

  /**
   * Read settings from an index.
   * This must only be applied to the JSON from a response to "<index>/_settings", which looks like:
   * {"indexname":{"settings":{"index.number_of_replicas":"1","index.number_of_shards":"1"}}}
   */
  def fromJson(settings: JsValue): Settings = settings match {
    case JsObject(Seq((_, json))) => settingsFormat.reads(json \ "settings") match {
      case JsSuccess(result, _) => result
      case JsError(errors) => throw ElasticSearchException(-1, "Bad settings: "+errors, settings)
    }
    case _ => throw ElasticSearchException(-1, "Bad settings received.", settings)
  }

  /* Formats, reads and writes are lazy, to prevent a problem with the initialization order.
   * The problem is caused by formatNullable and gives a NullPointerException.
   * Apparently, formatNullable does not find the implicit Writes if that is defined later.
   */

  implicit lazy val settingsFormat: Format[Settings]  = (
    (__ \ "index.number_of_shards").format(intStringFormat) and
    (__ \ "index.number_of_replicas").format(intStringFormat) and
    (__ \ "index.analysis").formatNullable[Analysis]
  )(Settings.apply, unlift(Settings.unapply))

}
