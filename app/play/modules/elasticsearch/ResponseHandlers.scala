package play.modules.elasticsearch

import play.api.libs.json.Reads
import play.api.libs.json.JsValue
import play.api.libs.ws.Response

object ResponseHandlers {
  
  val unitOrError = convertOrError(_ => ())

  def fromJsonOrError[T: Reads] = convertJsonOrError(_.as[T])

  def optJsonOrError[T: Reads] = convertJsonOrError(_.asOpt[T])

  def convertJsonOrError[T](converter: JsValue => T) =
    convertOrError[T](response => converter(response.json))

  def ifExists[T](converter: Response => Option[T]): Response => Option[T] = {
    case response @ Status(404) => None
    case response => converter(response)
  }
  
  def ifExistsFlag[T](converter: Response => Option[T]): Response => Option[T] = {
    response =>
      (response.json \ "exists").asOpt[Boolean].map(if (_) converter(response) else None).getOrElse(None)
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

}