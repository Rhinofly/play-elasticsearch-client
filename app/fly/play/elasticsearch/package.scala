package fly.play

import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads
import play.api.libs.ws.WS
import play.api.libs.ws.WSRequestHolder

package object elasticsearch {

  type RequestHolder = WSRequestHolder
  type Version = Long
  type Identifier = String
  type Parameter = (String, String)

  object Version extends (JsValue => Version) {
    def apply(json: JsValue): Long =
      reader.reads(json).asOpt
        .getOrElse(throw new RuntimeException("Could not retrieve version from " + json))

    val reader = (__ \ '_version).read[Version]
  }

  object Identifier extends (JsValue => Identifier) {
    def apply(json: JsValue): String =
      reader.reads(json).asOpt
        .getOrElse(throw new RuntimeException("Could not retrieve identifier from " + json))

    val reader = (__ \ '_id).read[Identifier]
  }

}