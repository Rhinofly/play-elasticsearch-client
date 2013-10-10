package play.modules.elasticsearch

import play.api.libs.json._
import play.api.libs.functional.syntax._

case class SearchResult[DocType] (
  index : String,
  docType : String,
  id : Identifier,
  version : Version,
  source : DocType
) {
  
  implicit def searchResultReads[DocType : Reads]: Reads[SearchResult[DocType]] = (
      (__ \ "_index").read[String] and
      (__ \ "_type").read[String] and
      (__ \ "_id").read[Identifier] and
      (__ \ "_version").read[Version] and
      (__ \ "_source").read[DocType]
    ).apply(SearchResult[DocType].apply _)

}
