package fly.play.elasticsearch.query

import play.api.libs.json.{JsString, Json, Reads, Writes}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import fly.play.elasticsearch.{EnumUtils, JsonUtils}

/*
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-nested-query.html
 */
case class NestedQuery(
    path: String,
    query: Query,
    scoreMode: ScoreMode.Value = NestedQuery.defaultScoreMode
) extends Query with JsonUtils {

  def toQueryDSL =
    Json.obj( "nested" ->
      toJsonObject(
        "path" -> JsString(path),
        "query" -> query.toQueryDSL,
        "score_mode" -> toJsonIfNot(scoreMode, NestedQuery.defaultScoreMode)
      )
    )

}

object ScoreMode extends Enumeration {
  val avg, total, max, none = Value
  implicit val enumReads: Reads[Value] = EnumUtils.enumReads(ScoreMode)
  implicit val enumWrites: Writes[Value] = EnumUtils.enumWrites
}

object NestedQuery {
  val defaultScoreMode = ScoreMode.avg
}