package play.modules.elasticsearch

import play.api.libs.json._

object Identifier extends (JsValue => Identifier) {
  val id = (__ \ '_id).read[Identifier].reads _
  def apply(json: JsValue): String =
    id(json).asOpt
      .getOrElse(throw new RuntimeException("Could not retrieve identifier from " + json))
}