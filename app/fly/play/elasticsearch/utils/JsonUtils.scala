package fly.play.elasticsearch.utils

import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.json.Json.JsValueWrapper
import scala.language.implicitConversions
import scala.util.Try

trait JsonUtils {
  
  /**
   * These functions are used to create Json objects from filtered sequences of (String, JsValue) tuples.
   * When the JsValue in a tuple is JsNull or JsUndefined or an empty JsObject, that tuple is considered not valid, and will be filtered out.
   */

  /**
   * Create a JsValue from `value`, which is valid if the `isValid` function applied to `value` is true.
   */
  protected def toJsonIfValid[T: Writes](value: T, isValid: T => Boolean): JsValue =
    if (isValid(value)) Json.toJson(value) else JsNull

  /**
   * Create a JsValue from `xform` applied to `value`, which is valid if the `isValid` function applied to `value` is true.
   */
  protected def toJsonIfValid[T, S: Writes](value: T, isValid: T => Boolean, xform: T => S): JsValue =
    if (isValid(value)) Json.toJson(xform(value)) else JsNull

  /**
   * Create a JsValue from `value`, which is valid if `value` is not equal to `default`.
   */
  protected def toJsonIfNot[T: Writes](value: T, default: T): JsValue =
    if (value != default) Json.toJson(value) else JsNull

  /**
   * Create a JsValue from `xform` applied to `value`, which is valid if `value` is not equal to `default`.
   */
  protected def toJsonIfNot[T, S: Writes](value: T, default: T, xform: T => S): JsValue =
    if (value != default) Json.toJson(xform(value)) else JsNull

  /**
   * Determines if a property (String, JsValue) is valid, by testing the JsValue in the second item.
   */
  protected def isValidJsonProperty(property: (String, JsValue)) =
    property match {
      case (_, obj: JsObject) => obj.fields.length > 0
      case (_, v) => (v != JsNull && !v.isInstanceOf[JsUndefined])
    }

  /**
   * Filters a series of properties, keeping only the valid ones.
   */
  protected def filterValid(properties: (String, JsValue)*) =
    properties.filter(isValidJsonProperty)

  /**
   * Create a Json object by filtering a series of properties.
   */
  protected def toJsonObject(properties: (String, JsValue)*) =
    JsObject(filterValid(properties:_*))

  /**
   * Format for an integer represented by a Json string.
   */
  val intStringFormat = new Format[Int] {
    def reads(jsValue: JsValue) = jsValue match {
      case JsString(value) => Try {
        JsSuccess(value.toInt)
      } getOrElse JsError("Not a number: "+value)
      case other => JsError("Expected a JsString containing a number, not "+other)
    }
    def writes(value: Int) = JsString(value.toString)
  }

  /**
   * JSON combinators do not (yet) support JSON objects with only one field.
   * This clever solution comes from EECOLOR (http://stackoverflow.com/questions/15042205/how-to-serialize-deserialize-case-classes-to-from-json-in-play-2-1).
   * Unfortunately it leaves a path in the JsSuccess, which we don't do in out `Format`s
  implicit class FormatBuilder[M[_], A](o: M[A]) {
    import play.api.libs.functional.InvariantFunctor
    import scala.language.higherKinds
    def singleField[B](f1: A => B, f2: B => A)(implicit fu: InvariantFunctor[M]) =
      fu.inmap[A, B](o, f1, f2)
  }
   */

  /**
   * Nullable formats with defaults.
   */
  def formatWithDefault[T : Format](path: JsPath, default: T): OFormat[T] =
    path.formatNullable[T].inmap[T](_.getOrElse(default), {x: T => Some(x)})

  /**
   * Make a JsResult[Seq[T]] from a Seq[JsResult[T]]. If there are errors, only a JsError is returned.
   */
  def foldJsResults[T](results: Seq[JsResult[T]]) : JsResult[Seq[T]] =
    results.foldRight[JsResult[Seq[T]]](JsSuccess(Seq.empty[T])) {
      case (JsSuccess(result, _), JsSuccess(results, _)) => JsSuccess(result +: results)
      case (JsSuccess(_, _), errors: JsError) => errors
      case (error: JsError, JsSuccess(_, _)) => error
      case (error: JsError, errors: JsError) => error ++ errors
    }

}

/**
 * Generic Reads, Writes and Format methods that can be used with any enumeration.
 * See http://perevillega.com/blog/2013/09/21/enums-to-json-in-scala/
 */
object EnumUtils {

  def enumReads[E <: Enumeration](enum: E): Reads[E#Value] =
    enumReadsWithTransform(enum, identity[String])

  def enumReadsWithTransform[E <: Enumeration](enum: E, xform: String => String): Reads[E#Value] =
    new Reads[E#Value] {
      def reads(json: JsValue): JsResult[E#Value] = json match {
        case JsString(s) => {
          try {
            JsSuccess(enum.withName(xform(s)))
          } catch {
            case _: NoSuchElementException =>
              JsError(s"Enumeration expected of type: '${enum.getClass}', but it does not appear to contain the value: '$s'")
          }
        }
        case _ => JsError("String value expected")
      }
  }

  def enumWrites[E <: Enumeration]: Writes[E#Value] =
    new Writes[E#Value] {
      def writes(v: E#Value): JsValue = JsString(v.toString)
    }

  def enumWritesWithTransform[E <: Enumeration](xform: String => String): Writes[E#Value] =
    new Writes[E#Value] {
      def writes(v: E#Value): JsValue = JsString(xform(v.toString))
    }

  def enumFormat[E <: Enumeration](enum: E): Format[E#Value] = {
    Format(enumReads(enum), enumWrites)
  }

}
