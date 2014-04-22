package fly.play.elasticsearch.analysis

import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.json.Reads._

case class AnalyzedToken(
  token: String,
  startOffset: Int,
  endOffset: Int,
  position: Int,
  tokenType: String
)

object AnalyzedToken {

  implicit def AnalyzedTokenReads: Reads[AnalyzedToken] = (
      (__ \ "token").read[String] and
      (__ \ "start_offset").read[Int] and
      (__ \ "end_offset").read[Int] and
      (__ \ "position").read[Int] and
      (__ \ "type").read[String]
    )(AnalyzedToken.apply _)

}
