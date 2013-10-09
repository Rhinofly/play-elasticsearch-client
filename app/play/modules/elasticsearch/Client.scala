package play.modules.elasticsearch

import scala.concurrent.Future
import play.api.libs.ws.Response
import play.api.libs.ws.WS
import play.api.http.HeaderNames
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json.JsObject

class Client(url: String) {

  def health: Future[Response] =
    WS.url(url + "/_cluster/health").get()

  def apply(indexName: String) = Index(indexName)
  def index = apply _

  case class Index(name: String) {
    val indexUrl = WS.url(url + s"/$name/")

    def create: Future[Response] =
      indexUrl
        .put(Array.empty[Byte])
        .map(responseOrError)

    def delete: Future[Response] =
      indexUrl
        .delete
        .map(responseOrError)

    def exists: Future[Boolean] =
      indexUrl
        .head
        .map(_.status == 200)
        
    def apply(typeName:String) = Type(typeName)
  }

  case class Type(name:String) {
    
    def put(doc:JsObject):Future[Response] = 
      ???
    
  }
  
  private def responseOrError(response: Response) =
    response.status match {
      case 200 => response
      case _ => {
        val json = response.json
        throw Client.ElasticSearchException((json \ "status").as[Int], (json \ "error").as[String])
      }
    }
}

object Client {
  case class ElasticSearchException(status: Int, message: String) extends RuntimeException(message: String)
}