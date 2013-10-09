package play.modules.elasticsearch

import scala.concurrent.Future

import play.api.http.ContentTypeOf
import play.api.http.Writeable
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.JsObject
import play.api.libs.json.JsValue
import play.api.libs.json.Reads
import play.api.libs.json.Writes
import play.api.libs.ws.Response
import play.api.libs.ws.WS

class Client(elasticSearchUrl: String) {

  val normalizedUrl =
    elasticSearchUrl + (if (elasticSearchUrl.last == '/') "" else '/')

  def url(path: String = "") = WS.url(normalizedUrl + path)

  def health: Future[JsObject] =
    url("_cluster/health").get().map(fromJsonOrError[JsObject])

  def apply(indexName: String) = Index(indexName)
  val index = apply _

  case class Index(name: String) {
    def url(implicit path: String = "") = Client.this.url(name + '/' + path)

    def create: Future[Unit] =
      url.put(Array.empty[Byte]).map(unitOrError)

    def delete: Future[Unit] =
      url.delete.map(unitOrError)

    def exists: Future[Boolean] =
      url.head.map(_.status == 200)

    def apply(typeName: String) = Type(typeName)

    case class Type(name: String) {

      def url(path: String) = Index.this.url(name + '/' + path)

      def put[T : Writes](id: String, doc: T)(implicit w:Writeable[T], c:ContentTypeOf[T]): Future[Version] = {
        url(id).put(doc).map(convertJsonOrError(Version))
      }

    }
  }

  private val unitOrError = convertOrError(_ => ()) _
  private def fromJsonOrError[T: Reads] = convertJsonOrError(_.as[T])
  private def convertJsonOrError[T](converter: JsValue => T) =
    convertOrError[T](r => converter(r.json)) _

  private def convertOrError[T](converter: Response => T)(response: Response): T =
    response.status match {
      case 200 | 201 => converter(response)
      case status =>
        val json = response.json
        val possibleException =
          for {
            status <- (json \ "status").asOpt[Int]
            error <- (json \ "error").asOpt[String]
          } yield ElasticSearchException(status, error)

        throw possibleException.getOrElse(new RuntimeException(s"Unknown status code $status with body: ${response.body}"))
    }
}