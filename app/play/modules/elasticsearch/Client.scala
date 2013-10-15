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
import play.api.libs.json.JsSuccess
import play.api.libs.json.JsError
import play.api.libs.json.Json
import play.modules.elasticsearch.query.Query

class Client(elasticSearchUrl: String) {

  import ResponseHandlers._

  val normalizedUrl =
    elasticSearchUrl + (if (elasticSearchUrl.last == '/') "" else '/')

  def url(path: String = "") = WS.url(normalizedUrl + path)

  def health: Future[JsObject] = health()

  def health(parameters: Parameter*): Future[JsObject] =
    url("_cluster/health")
      .withQueryString(parameters: _*)
      .get()
      .map(fromJsonOrError[JsObject])

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

      def url(implicit path: String = "") = Index.this.url(name + '/' + path)
      
      /* Document APIs */

      def put[T](id: Identifier, doc: T, parameters: Parameter*)(implicit writer: Writes[T]): Future[Version] =
        url(id)
          .withQueryString(parameters: _*)
          .put(writer.writes(doc))
          .map(convertJsonOrError(Version))

      def post[T](doc: T, parameters: Parameter*)(implicit writer: Writes[T]): Future[(Version, Identifier)] =
        url
          .withQueryString(parameters: _*)
          .post(writer.writes(doc))
          .map(convertJsonOrError(json => Version(json) -> Identifier(json)))

      def get[T: Reads](id: Identifier, parameters: Parameter*): Future[Option[(Version, T)]] =
        url(id)
          .withQueryString(parameters: _*)
          .get()
          .map(ifExists(ifExistsFlag(optJsonOrError[(Version, T)])))

      def delete[T](id: Identifier, parameters: Parameter*): Future[Boolean] =
        url(id)
          .withQueryString(parameters: _*)
          .delete()
          .map(convertJsonOrError(json => (json \ "ok").as[Boolean]))
          
      def updateDoc[T](id: Identifier, doc: T, parameters: Parameter*)(implicit writer: Writes[T]): Future[Version] =
        url(id+"/_update")
          .withQueryString(parameters: _*)
          .post(Json.obj("doc" -> writer.writes(doc)))
          .map(convertJsonOrError(Version))
          
      // No need for method updateScript anticipated.
    
      /* Search APIs */
      
      def search[T: Reads](query: Query, parameters: Parameter*): Future[Option[(SearchResult, List[T])]] = {
        url("_search")
          .withQueryString(parameters: _*)
          .post(query.toJson) // GET does not accept a http-body in Play2.1.
          .map(ifExists(optJsonOrError[(SearchResult, List[T])]))
      }
      
    }
  }
}