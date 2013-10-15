package play.modules.elasticsearch

import play.api.libs.json._
import play.api.libs.functional.syntax._

case class SearchResult (took: Int, timed_out: Boolean, hits : Int, max_score : Option[Double])

object SearchResult {

  val searchResultReads: Reads[SearchResult] = (
      (__ \ "took").read[Int] and
      (__ \ "timed_out").read[Boolean] and
      (__ \ "hits" \ "total").read[Int] and
      (__ \ "hits" \ "max_score").read[Option[Double]]
    )(SearchResult.apply _)

  def searchHitsReads[T: Reads]: Reads[List[T]] =
    (__ \ "hits" \ "hits").read[List[T]]

}