package play.modules.elasticsearch

import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._

case class SearchResult[T] (
  took: Int,
  timed_out: Boolean,
  hits_total : Int,
  max_score : Option[Double],
  hits: List[ResultDocument[T]] /* Length may be less than hits_total if query has from and size properties. */
)

case class ResultDocument[T] (
  id: Identifier,
  score: Option[Double],
  source: T
)

object SearchResult {

  def searchResultReads[T: Reads]: Reads[SearchResult[T]] = (
      (__ \ "took").read[Int] and
      (__ \ "timed_out").read[Boolean] and
      (__ \ "hits" \ "total").read[Int] and
      (__ \ "hits" \ "max_score").read[Option[Double]] and
      (__ \ "hits" \ "hits").read( list[ResultDocument[T]](ResultDocument.resultDocumentReads[T]) )
    )(SearchResult.apply[T] _)
}

object ResultDocument {
  
  def resultDocumentReads[T: Reads]: Reads[ResultDocument[T]] = (
      (__ \ "_id").read[Identifier] and
      (__ \ "_score").read[Option[Double]] and
      (__ \ "_source").read[T]
    )(ResultDocument.apply[T] _)

}
