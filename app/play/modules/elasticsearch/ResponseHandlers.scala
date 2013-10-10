package play.modules.elasticsearch

import play.api.libs.json.Reads
import play.api.libs.json.JsValue
import play.api.libs.ws.Response

object ResponseHandlers {
  
  val unitOrError = convertOrError(_ => ())

  def fromJsonOrError[T: Reads] = convertJsonOrError(_.as[T])

  def convertJsonOrError[T](converter: JsValue => T) =
    convertOrError[T](response => converter(response.json))

  def ifExists[T](converter: Response => T): Response => Option[T] = {
    case response @ Status(404) if existsIsFalse(response) => None
    case response => Some(converter(response))
  }

  def convertOrError[T](converter: Response => T): Response => T = {
    case response @ Status(200 | 201) =>
      converter(response)
    case response @ Status(status) =>
      val json = response.json
      val possibleException =
        for {
          status <- (json \ "status").asOpt[Int]
          error <- (json \ "error").asOpt[String]
        } yield ElasticSearchException(status, error, json)

      throw possibleException.getOrElse(new RuntimeException(s"Unknown status code $status with body: ${response.body}"))
  }

  object Status {
    def unapply(response: Response): Option[Int] =
      Some(response.status)
  }

  def existsIsFalse(response: Response): Boolean = {
    (response.json \ "exists").asOpt[Boolean].map(_ == false).getOrElse(false)
  }
}