package play.modules.elasticsearch

import play.api.libs.json._

object Version extends (JsValue => Version) {
  val version = (__ \ '_version).read[Version].reads _
  def apply(json: JsValue): Long =
    version(json).asOpt
      .getOrElse(throw new RuntimeException("Could not retrieve version from " + json))
}