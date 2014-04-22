package play.modules.elasticsearch

import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.json.Reads._

/**
 * A search result including found documents in `hits`.
 * The length of the `hits` list may be less than `hits_total` if the query has `from` and `size` properties.
 */
case class SearchResult[T] (
  took: Int,
  timedOut: Boolean,
  hitsTotal : Int,
  maxScore : Option[Double],
  hits: List[ResultDocument[T]]
)

/**
 * A single document in a search result.
 * The  `highlight` map is optional, and only present if the query asks for highlighting. It maps field names to sequences of highlighted fragments.
 */
case class ResultDocument[T] (
  id: Identifier,
  version: Option[Version],
  score: Option[Double],
  source: T,
  highlight: Option[Map[String, Seq[String]]]
) {

  /**
   * Gets a highlight list for a field.
   * Returns the empty list if no highlights were found, or if the query did not ask for highlighting.
   */
  def highlightFor(field: String): Seq[String] =
    highlight.flatMap(_.get(field)).getOrElse(Seq.empty)

}

object SearchResult {

  implicit def searchResultReads[T: Reads]: Reads[SearchResult[T]] = (
      (__ \ "took").read[Int] and
      (__ \ "timed_out").read[Boolean] and
      (__ \ "hits" \ "total").read[Int] and
      (__ \ "hits" \ "max_score").readNullable[Double] and
      (__ \ "hits" \ "hits").read( list[ResultDocument[T]] )
    )(SearchResult.apply[T] _)

}

object ResultDocument {

  implicit def resultDocumentReads[T: Reads]: Reads[ResultDocument[T]] = (
      (__ \ "_id").read[Identifier] and
      (__ \ "_version").readNullable[Version] and
      (__ \ "_score").readNullable[Double] and
      (__ \ "_source").read[T] and
      (__ \ "highlight").readNullable[Map[String, Seq[String]]](highlightReads)
    )(ResultDocument.apply[T] _)

  def highlightReads: Reads[Map[String, Seq[String]]] = Reads { json => JsSuccess(
    json.as[JsObject].fields.map({case (field, hilites) =>
      (field, hilites.as[JsArray].value.map(js => js.as[String]))
    }).toMap
  )}

}
