package play.modules.elasticsearch

import play.api.libs.json._
import play.api.libs.json.util._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import play.api.data.validation.ValidationError

/**
 * Settings for ES indices.
 * See http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/indices-create-index.html
 */
case class Settings(
    nrOfShards: Int = 1,
    nrOfReplicas: Int = 1,
    analysis: Option[Analysis] = None
  ) {

  def toJson =
    Json.obj(
      "settings" -> Settings.settingsFormat.writes(this)
    )

  def toJsonWithMappings(mappings: Seq[Mapping]) =
    Json.obj(
      "settings" -> Settings.settingsFormat.writes(this),
      "mappings" -> JsObject(mappings.map{Mapping.jsonTuple})
    )

}

case class Analysis(
    analyzers: Seq[Analyzer] = Seq.empty,
    tokenizers: Seq[Tokenizer] = Seq.empty,
    filters: Seq[Filter] = Seq.empty
  )

trait Analyzer {
  def name: String
}

case class StandardAnalyzer(
    name: String,
    stopwords: Seq[String] = Seq.empty,
    maxTokenLength: Int = 255
  ) extends Analyzer

case class CustomAnalyzer(
    name: String,
    tokenizer: String,
    filter: Seq[String]
  ) extends Analyzer

case class Tokenizer(
    name: String,
    `type`: String
  )

case class Filter(
    name: String,
    `type`: String
  )


object Settings extends JsonUtils {

  /**
   * Read settings from an index.
   * This must only be applied to the JSON from a response to "<index>/_settings", which looks like:
   * {"indexname":{"settings":{"index.number_of_replicas":"1","index.number_of_shards":"1"}}}
   */
  def fromJson(settings: JsValue): Settings = settings match {
    case JsObject(Seq((_, json))) => settingsFormat.reads(json \ "settings") match {
      case JsSuccess(result, _) => result
      case JsError(errors) => throw ElasticSearchException(-1, "Bad settings: "+errors, settings)
    }
    case _ => throw ElasticSearchException(-1, "Bad settings received.", settings)
  }

  /* Formats, reads and writes are lazy, to prevent a problem with the initialization order.
   * The problem is caused by formatNullable and gives a NullPointerException.
   * Apparently, formatNullable does not find the implicit Writes if that is defined later.
   */

  implicit lazy val settingsFormat: Format[Settings]  = (
    (__ \ "index.number_of_shards").format(intStringFormat) and
    (__ \ "index.number_of_replicas").format(intStringFormat) and
    (__ \ "analysis").formatNullable[Analysis]
  )(Settings.apply, unlift(Settings.unapply))

  implicit lazy val analysisFormat: Format[Analysis] = (
    (__ \ "analyzer").format[Seq[Analyzer]] and
    (__ \ "tokenizer").format[Seq[Tokenizer]] and
    (__ \ "filter").format[Seq[Filter]]
  )(Analysis.apply, unlift(Analysis.unapply))

  implicit lazy val analyzerListReads: Reads[Seq[Analyzer]] = Reads(json =>
    JsSuccess(json.as[JsObject].fieldSet.map{case(name, js) =>
      (js \ "type") match {
          case JsString("standard") =>
            StandardAnalyzer(
              name = name,
              stopwords = (js \ "stopwords").as[Seq[String]],
              maxTokenLength = (js \ "max_token_length").as[Int]
            )
          case JsUndefined(error) =>
            CustomAnalyzer(
              name = name,
              tokenizer = (js \ "tokenizer").as[String],
              filter = (js \ "filter").as[Seq[String]]
            )
          case other => throw new ElasticSearchException(-1, "Unknown analyzer type.", other)
        }
      }.toSeq)
  )

  implicit lazy val analyzerListWrites: Writes[Seq[Analyzer]] = Writes({analyzers: Seq[Analyzer] =>
    JsObject(
      analyzers.map{nlzr => nlzr match {
        case StandardAnalyzer(name, stopwords, maxTokenLength) =>
          name -> toJsonObject(
              "type" -> JsString("standard"),
              "stopwords" -> JsArray(stopwords.map{JsString(_)})
            )
        case CustomAnalyzer(name, tokenizer, filter) =>
          name -> Json.obj(
          "tokenizer" -> tokenizer,
          "filter" -> JsArray(filter.map{JsString(_)})
        )
      }}
    )
  })

  implicit lazy val tokenizerListReads: Reads[Seq[Tokenizer]] = Reads(json =>
    JsSuccess(json.as[JsObject].fieldSet.map{case(name, js) =>
      Tokenizer(
        name = name,
        `type` = (js \ "type").as[String]
      )
    }.toSeq)
  )

  implicit lazy val tokenizerListWrites: Writes[Seq[Tokenizer]] = Writes({tokenizers: Seq[Tokenizer] =>
    JsObject(
      tokenizers.map{tknzr => (tknzr.name, Json.obj(
        "type" -> tknzr.`type`
      ))}
    )
  })

  implicit lazy val filterListReads: Reads[Seq[Filter]] = Reads(json =>
    JsSuccess(json.as[JsObject].fieldSet.map{case(name, js) =>
      Filter(
        name = name,
        `type` = (js \ "type").as[String]
      )
    }.toSeq)
  )

  implicit lazy val filterListWrites: Writes[Seq[Filter]] = Writes({filters: Seq[Filter] =>
    JsObject(
      filters.map{fltr => (fltr.name, Json.obj(
        "type" -> fltr.`type`
      ))}
    )
  })

}
