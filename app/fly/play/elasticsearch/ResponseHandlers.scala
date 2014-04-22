package fly.play.elasticsearch

import play.api.libs.json.{JsObject, JsValue, Reads, __}
import play.api.libs.functional.syntax._
import play.api.libs.ws.Response
import scala.annotation.implicitNotFound

object ResponseHandlers {

  private def sourceReader[T: Reads] = (__ \ '_source).read[T]
  private def fieldsReader[T: Reads] = (__ \ 'fields).read[T]
  def sourceOrFieldsReader[T: Reads] = sourceReader[T] or fieldsReader[T]
  implicit def versionAndDocumentReader[T: Reads]: Reads[(Version, T)] = {
    (Version.reader and sourceOrFieldsReader[T]).tupled
  }

  val unitOrError: Response => Unit = convertOrError(_ => ())

  def fromJsonOrError[T: Reads] = convertJsonOrError(_.as[T])

  def fromJsonOptOrError[T: Reads] = convertJsonOrError(_.asOpt[T])

  def convertJsonOrError[T](converter: JsValue => T) =
    convertOrError[T](response => converter(response.json))

  def check[T](condition: Response => Boolean, converter: Response => Option[T]): Response => Option[T] = { response =>
    if (condition(response)) converter(response) else None
  }

  /* Some functions to test special conditions. */

  // A result is not found if the status is 404, but if the found property in the JSON is not "false", continue to the error.
  def resultExists(response: Response): Boolean =
    found(response) || (response.json \ "found").asOpt[Boolean] != Some(false)

  def resultNotEmpty(response: Response) : Boolean =
    response.json != JsObject(Seq.empty)

  def found(response: Response) : Boolean =
    response.status != 404

  def foundOrError(response: Response) : Boolean =
    if (!found(response)) false else convertOrError(_ => true)(response)

  /**
   * Apply a converter if the response is successful. Otherwise throw an exception.
   */
  def convertOrError[T](converter: Response => T): Response => T = {
    case response @ Status(200 | 201) => converter(response)
    case response @ Status(status) =>
      val json = response.json
      val possibleException =
        for {
          status <- (json \ "status").asOpt[Int]
          error <- (json \ "error").asOpt[String]
        } yield ElasticSearchException(status, error, json)
      throw possibleException.getOrElse(unknownStatusCode(status, response))
  }

  def unknownStatusCode(status: Int, response: Response) =
    new RuntimeException(s"Unknown status code $status with body: ${response.body}")

  object Status {
    def unapply(response: Response): Option[Int] =
      Some(response.status)
  }

}