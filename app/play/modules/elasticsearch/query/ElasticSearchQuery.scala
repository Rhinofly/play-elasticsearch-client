package play.modules.elasticsearch.query

import play.api.libs.json._

/**
 * An ElasticSearchQuery has one of the subclasses of Query (http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-request-query.html),
 * plus extra properties accepted by the 'request body search' (http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-request-body.html).
 */

case class ElasticSearchQuery(
    query: Query,
    properties: Seq[(String, JsValue)] = Seq.empty
  ) {

  /* Methods to add specific properties to a query. */

  /**
   * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-request-version.html
   */
  def withVersion(version: Boolean) =
    withProperty("version" -> version)

  /**
   * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-request-from-size.html
   */
  def withFrom(from: Int) =
    withProperty("from" -> from)

  /**
   * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-request-from-size.html
   */
  def withSize(size: Int) =
    withProperty("size" -> size)

  /**
   * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-request-sort.html
   */
  def withSort(sorting: Seq[Sort]) =
    withProperty("sort" -> sorting)

  /**
   * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-request-highlighting.html
   */
  def withHighlight(highlight: Highlight) =
    withProperty("highlight" -> highlight)

  def withHighlight(highlights: HighlightField*) =
    withProperty("highlight" -> Highlight(highlights))

  /**
   * Add any property to a query.
   */
  private def withProperty[T](property: (String, T))(implicit writer:Writes[T]) = {
    val (key, value) = property
    copy(properties = properties :+ (key -> writer.writes(value)))
  }

  /**
   * Produce the JSON sent to ES for the query.
   */
  def toJson: JsObject =
    Json.obj("query" -> query.toQueryDSL) ++ JsObject(properties)

}
