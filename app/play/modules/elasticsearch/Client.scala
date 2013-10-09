package play.modules.elasticsearch

import scala.concurrent.Future
import play.api.libs.ws.Response
import play.api.libs.ws.WS
import play.api.http.HeaderNames
import play.api.libs.concurrent.Execution.Implicits._

class Client(url: String) {

  def health: Future[Response] =
    WS.url(url + "/_cluster/health").get()

  def createIndex(name: String): Future[Response] =
    indexUrl(name)
      .put(Array.empty[Byte])
      .map(responseOrError)

  def deleteIndex(name: String): Future[Response] =
    indexUrl(name)
      .delete
      .map(responseOrError)

  def existsIndex(name: String): Future[Boolean] =
    indexUrl(name)
      .head
      .map(_.status == 200)

  private def indexUrl(name:String) = 
    WS.url(url + s"/$name/")
      
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