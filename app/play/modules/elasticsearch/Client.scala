package play.modules.elasticsearch

import ResponseHandlers.{convertJsonOrError, found, fromJsonOrError, ifExists, unitOrError}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.{JsNull, JsObject, Json}
import play.api.libs.json.{Reads, Writes}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.libs.ws.{Response, WS}
import play.api.libs.ws.Implicits._
import play.modules.elasticsearch.mapping.Mapping
import play.modules.elasticsearch.query.ElasticSearchQuery
import scala.concurrent.Future
import scala.language.existentials
import scala.util.{Failure, Success, Try}

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

  /* A client can connect to several ES indexes. */
  case class Index(name: String) {

    /* Index APIs: http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/indices.html */

    def url(implicit path: String = "") = Client.this.url(name + '/' + path)

    /* Create can have settings or mappings parameters, or both. */

    def create(): Future[Unit] =
      url.put(Array.empty[Byte]).map(unitOrError)

    def create(settings: Settings): Future[Unit] =
      url.post(settings.toJson).map(unitOrError)

    def create(mappings: Seq[Mapping]): Future[Unit] =
      url.post(Mapping.jsonForMappings(mappings)).map(unitOrError)

    def create(settings: Settings, mappings: Seq[Mapping]): Future[Unit] =
      url.post(settings.toJsonWithMappings(mappings)).map(unitOrError)

    def delete(): Future[Boolean] =
      url.delete.map(found)

    def exists: Future[Boolean] =
      url.head.map(found)

    /* Retrieve mappings for all types. */
    def mappings: Future[Seq[Mapping]] =
      url("_mapping").get.map(convertJsonOrError(Mapping.mappingsFromJson))

    /* Refresh will commit the index and make all documents findable. */
    def refresh(): Future[Unit] =
      url("_refresh").post("").map(unitOrError)

    def apply(typeName: String) = Type(typeName)

    /* An index can contain several types. */
    case class Type(name: String) {

      private def url(path: String, parameters: Parameter*) =
        Index.this.url(name + '/' + path).withQueryString(parameters: _*)

      private def url(parameters: Parameter*): RequestHolder =
        url("", parameters: _*)

      private def putWithHandler[T, R](handler: Response => R)(id: Identifier, doc: T, parameters: Parameter*)(implicit writer: Writes[T]): Future[R] =
        Try(writer.writes(doc)) match {
          case Success(body) => url(id, parameters: _*).put(body).map(handler)
          case Failure(error) => Future.failed(ElasticSearchException(500, "Cannot make JSON: "+error, JsNull))
        }

      private def postWithHandler[T, R](handler: Response => R)(doc: T, parameters: Parameter*)(implicit writer: Writes[T]): Future[R] =
        Try(writer.writes(doc)) match {
          case Success(body) => url(parameters: _*).post(body).map(handler)
          case Failure(error) => Future.failed(ElasticSearchException(500, "Cannot make JSON: "+error, JsNull))
        }

      /* Define a mapping for this type: http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/indices-put-mapping.html */
      def create(mapping: Mapping)(implicit mappingWrites: Writes[Mapping]): Future[Unit] =
        url("_mapping").put(mappingWrites.writes(mapping)).map(unitOrError)

      /* Retrieve the mapping for this type, or throw an exception if it does not exist: http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/indices-get-mapping.html */
      def mapping(implicit mappingReads: Reads[Mapping]) : Future[Mapping] =
        url("_mapping").get().map(fromJsonOrError(mappingReads))

      /* Retrieve the mapping as an option, returning None if there is no mapping for the type. */
      def mappingOpt(implicit mappingReads: Reads[Mapping]) : Future[Option[Mapping]] =
        url("_mapping").get().map(response => if(response.status != 404) Some(fromJsonOrError(mappingReads)(response)) else None)

      /* Index a document: http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/docs-index_.html */
      def index[T: Writes](id: Identifier, doc: T, parameters: Parameter*): Future[Unit] =
        putWithHandler(unitOrError)(id, doc, parameters: _*)

      def indexV[T: Writes](id: Identifier, doc: T, parameters: Parameter*): Future[Version] =
        putWithHandler(convertJsonOrError(Version))(id, doc, parameters: _*)

      /* Index (automatic id generation): http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/docs-index_.html */
      def index[T: Writes](doc: T, parameters: Parameter*): Future[Identifier] =
        postWithHandler(convertJsonOrError(Identifier))(doc, parameters: _*)

      def indexV[T](doc: T, parameters: Parameter*)(implicit writer: Writes[T]): Future[(Identifier, Version)] =
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
      def search[T: Reads](query: ElasticSearchQuery, parameters: Parameter*): Future[SearchResult[T]] =
        url("_search", parameters: _*)
          .post(query.toJson) // GET does not accept a http-body in Play2.1 (but later we added play.api.libs.ws.Implicits).
          .map(fromJsonOrError[SearchResult[T]])

      /* Delete documents corresponding to a query. See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/docs-delete-by-query.html */
      def deleteByQuery[T](query: ElasticSearchQuery, parameters: Parameter*): Future[Boolean] =
        url("_query", parameters: _*).delete(query.toJson \ "query").map(found)

    }
  }
}