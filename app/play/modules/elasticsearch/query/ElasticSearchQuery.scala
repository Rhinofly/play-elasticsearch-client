package play.modules.elasticsearch.query

import play.api.libs.json._

/* Query using ElasticSearch Query-DSL as described in http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl.html.
 * This has different kinds of queries as sub-classes, see http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-queries.html.
 */

case class ElasticSearchQuery(query: Query, properties: Seq[(String, JsValue)] = Seq.empty) {

  def withVersion(version: Boolean) =
    withProperty("version" -> version)

  def withFrom(from: Int) =
    withProperty("from" -> from)

  def withSize(size: Int) =
    withProperty("size" -> size)

  def withProperty[T](property: (String, T))(implicit writer:Writes[T]) = {
    val (key, value) = property
    copy(properties = properties :+ (key -> writer.writes(value)))
  }
  
  def toJson: JsObject =
    Json.obj("query" -> query.toQueryDSL) ++ JsObject(properties)

}
