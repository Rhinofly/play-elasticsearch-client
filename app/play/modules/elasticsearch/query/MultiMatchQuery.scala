package play.modules.elasticsearch.query

import play.api.libs.json._
import play.modules.elasticsearch.JsonUtils
import play.modules.elasticsearch.EnumUtils

/*
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/query-dsl-multi-match-query.html
 */
case class MultiMatchQuery(
  fields: Seq[String],
  value: String,
  operator: Operator.Value = MultiMatchQuery.defaultOperator,
  matchType : MultiMatchType.Value = MultiMatchQuery.defaultMultiMatchType,
  fuzziness: Double = MultiMatchQuery.defaultFuzziness,
  slop: Int = MultiMatchQuery.defaultSlop
) extends Query with JsonUtils {

  def toQueryDSL =
    Json.obj( "multi_match" ->
      toJsonObject(
        "fields" -> Json.toJson(fields),
        "query" -> JsString(value),
        "operator" -> toJsonIfNot(operator, MultiMatchQuery.defaultOperator),
        "type" -> toJsonIfNot(matchType, MultiMatchQuery.defaultMultiMatchType),
        "fuzziness" -> toJsonIfNot(fuzziness, MultiMatchQuery.defaultFuzziness),
        "slop" -> toJsonIfNot(slop, MultiMatchQuery.defaultSlop)
      )
    )

}

/**
 * Version 0.9.x: The query accepts all the options that a regular match query accepts.
 * For the `type` option, this will change in 1.1.0.
 */
object MultiMatchType extends Enumeration {
  val boolean, phrase, phrase_prefix = Value
  implicit val enumReads: Reads[Value] = EnumUtils.enumReads(MultiMatchType)
  implicit val enumWrites: Writes[Value] = EnumUtils.enumWrites
}

object MultiMatchQuery {
  val defaultOperator: Operator.Value = Operator.or
  val defaultMultiMatchType: MultiMatchType.Value = MultiMatchType.boolean
  val defaultFuzziness: Double = -1.0
  val defaultSlop: Int = 0
}
