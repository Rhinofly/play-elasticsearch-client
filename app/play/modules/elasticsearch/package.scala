package play.modules

import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads

package object elasticsearch {

  type Version = Long
  type Identifier = String
  type Parameter = (String, String)

  object Version extends (JsValue => Version) {
    def apply(json: JsValue): Long =
      versionReader.reads(json).asOpt
        .getOrElse(throw new RuntimeException("Could not retrieve version from " + json))
  }

  object Identifier extends (JsValue => Identifier) {
    def apply(json: JsValue): String =
      idReader.reads(json).asOpt
        .getOrElse(throw new RuntimeException("Could not retrieve identifier from " + json))
  }

  private val idReader = (__ \ '_id).read[Identifier]
  private val versionReader = (__ \ '_version).read[Version]
  private def sourceReader[T: Reads] = (__ \ '_source).read[T]
  private def fieldsReader[T: Reads] = (__ \ 'fields).read[T]
  private def sourceOrFieldsReader[T: Reads] = 
    sourceReader[T] or fieldsReader[T]

  implicit def versionAndDocumentReader[T: Reads]: Reads[(Version, T)] = (
    versionReader and
    sourceOrFieldsReader[T]).tupled
}