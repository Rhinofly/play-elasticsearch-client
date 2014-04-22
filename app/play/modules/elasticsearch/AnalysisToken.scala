package play.modules.elasticsearch

import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.json.Reads._

case class AnalysisToken(
  token: String,
  startOffset: Int,
  endOffset: Int,
  position: Int,
  tokenType: String
)

object AnalysisToken {

  implicit def analysisTokenReads: Reads[AnalysisToken] = (
      (__ \ "token").read[String] and
      (__ \ "start_offset").read[Int] and
      (__ \ "end_offset").read[Int] and
      (__ \ "position").read[Int] and
      (__ \ "type").read[String]
    )(AnalysisToken.apply _)

}
