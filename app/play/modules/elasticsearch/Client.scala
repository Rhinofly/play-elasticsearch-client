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
import play.api.libs.json.JsString

import scala.language.existentials

class Client(elasticSearchUrl: String) {

  import ResponseHandlers._

  val normalizedUrl =
    elasticSearchUrl + (if (elasticSearchUrl.last == '/') "" else '/')

  def url(path: String = "") = WS.url(normalizedUrl + path)

  def health: Future[JsObject] = health()

  /* Health: http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/cluster-health.html */
  def health(parameters: Parameter*): Future[JsObject] =
    url("_cluster/health")
      .withQueryString(parameters: _*)
      .get()
      .map(fromJsonOrError[JsObject])

  def apply(indexName: String) = Index(indexName)
  val index = apply _

  case class Index(name: String) {
    
    /* Index APIs: http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/indices.html */

    def url(implicit path: String = "") = Client.this.url(name + '/' + path)

    def create(): Future[Unit] =
      url.put(Array.empty[Byte]).map(unitOrError)

    def delete(): Future[Boolean] =
      url.delete.map(found)

    def exists: Future[Boolean] =
      url.head.map(found)

    /* Refresh will commit the index and make all documents findable. */
    def refresh(): Future[Unit] =
      url("_refresh").post("").map(unitOrError)

    def apply(typeName: String) = Type(typeName)

    case class Type(name: String) {

      def url(path: String, parameters: Parameter*) =
        Index.this.url(name + '/' + path).withQueryString(parameters: _*)

      def url(parameters: Parameter*): RequestHolder =
        url("", parameters: _*)

      def putWithHandler[T, R](handler: Response => R)(id: Identifier, doc: T, parameters: Parameter*)(implicit writer: Writes[T]): Future[R] =
        url(id, parameters: _*).put(writer.writes(doc)).map(handler)

      def postWithHandler[T, R](handler: Response => R)(doc: T, parameters: Parameter*)(implicit writer: Writes[T]): Future[R] =
        url(parameters: _*).post(writer.writes(doc)).map(handler)

      /* Index: http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/docs-index_.html */
      def put[T: Writes](id: Identifier, doc: T, parameters: Parameter*): Future[Unit] =
        putWithHandler(unitOrError)(id, doc, parameters: _*)

      def putV[T: Writes](id: Identifier, doc: T, parameters: Parameter*): Future[Version] =
        putWithHandler(convertJsonOrError(Version))(id, doc, parameters: _*)

      /* Index (automatic id generation): http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/docs-index_.html */
      def post[T: Writes](doc: T, parameters: Parameter*): Future[Identifier] =
        postWithHandler(convertJsonOrError(Identifier))(doc, parameters: _*)

      def postV[T](doc: T, parameters: Parameter*)(implicit writer: Writes[T]): Future[(Identifier, Version)] =
        postWithHandler(convertJsonOrError(json => (Identifier(json) -> Version(json))))(doc, parameters: _*)

      /* Get: http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/docs-get.html */
      def get[T: Reads](id: Identifier, parameters: Parameter*): Future[Option[T]] =
        url(id, parameters: _*)
          .get().map(ifExists(fromJsonOrError(sourceOrFieldsReader[T])))

      def getV[T: Reads](id: Identifier, parameters: Parameter*): Future[Option[(Version, T)]] =
        url(id, parameters: _*)
          .get().map(ifExists(fromJsonOrError[(Version, T)]))

      /* Delete: http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/docs-delete.html */
      def delete[T](id: Identifier, parameters: Parameter*): Future[Boolean] =
        url(id, parameters: _*)
          .delete()
          .map(found)

      /* Update: www.elasticsearch.org/guide/en/elasticsearch/reference/current/docs-update.html */
      def update[T](id: Identifier, doc: T, parameters: Parameter*)(implicit writer: Writes[T]): Future[Unit] =
        url(id + "/_update", parameters: _*)
          .post(Json.obj("doc" -> writer.writes(doc)))
          .map(unitOrError)

      def updateV[T](id: Identifier, doc: T, parameters: Parameter*)(implicit writer: Writes[T]): Future[Version] =
        url(id + "/_update", parameters: _*)
          .post(Json.obj("doc" -> writer.writes(doc)))
          .map(convertJsonOrError(Version))

      // No need for method updateScript anticipated.

      /* Search APIs: http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search.html */

      def search[T: Reads](query: Query, parameters: Parameter*): Future[SearchResult[T]] =
        url("_search", parameters: _*)
          .post(query.toJson) // GET does not accept a http-body in Play2.1.
          .map(fromJsonOrError[SearchResult[T]])

    }
  }
}