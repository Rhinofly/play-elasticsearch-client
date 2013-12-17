package play.modules.elasticsearch

import scala.language.implicitConversions
import play.api.libs.json._
import play.api.libs.json.Json.JsValueWrapper
import scala.util.Try

trait JsonUtils {

  /*
   * These functions are used to create Json objects from filtered sequences of (String, JsValue) tuples.
   * When the JsValue in a tuple is JsNull, that tuple is considered not valid, and will be filtered out.
   */

  protected def toJsonIfValid[T: Writes](value: T, isValid: T => Boolean): JsValue =
    if (isValid(value)) Json.toJson(value) else JsNull

  protected def toJsonIfNot[T: Writes](value: T, default: T): JsValue =
    if (value != default) Json.toJson(value) else JsNull

  protected def isValidJsonProperty(property: (String, JsValue)) =
    property match {
      case (k, v) => (v != JsNull)
    }

  protected def filterValid(properties: (String, JsValue)*) =
    properties.filter(isValidJsonProperty)

  protected def toJsonObject(properties: (String, JsValue)*) =
    JsObject(filterValid(properties:_*))

  val intStringFormat = new Format[Int] {
    def reads(jsValue: JsValue) = jsValue match {
      case JsString(value) => Try {
        JsSuccess(value.toInt)
      } getOrElse JsError("Not a number: "+value)
      case other => JsError("Expected a JsString containing a number, not "+other)
    }
    def writes(value: Int) = JsString(value.toString)
  }

}

/**
 * Generic Reads, Writes and Format methods that can be used with any enumeration.
 * See http://perevillega.com/blog/2013/09/21/enums-to-json-in-scala/
 */
object EnumUtils {
  def enumReads[E <: Enumeration](enum: E): Reads[E#Value] =
    new Reads[E#Value] {
      def reads(json: JsValue): JsResult[E#Value] = json match {
        case JsString(s) => {
          try {
            JsSuccess(enum.withName(s))
          } catch {
            case _: NoSuchElementException =>
              JsError(s"Enumeration expected of type: '${enum.getClass}', but it does not appear to contain the value: '$s'")
          }
        }
        case _ => JsError("String value expected")
      }
  }

  implicit def enumWrites[E <: Enumeration]: Writes[E#Value] =
    new Writes[E#Value] {
      def writes(v: E#Value): JsValue = JsString(v.toString)
    }

  implicit def enumFormat[E <: Enumeration](enum: E): Format[E#Value] = {
    Format(enumReads(enum), enumWrites)
  }
}
