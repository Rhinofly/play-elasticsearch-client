package play.modules.elasticsearch

import org.specs2.mutable.Specification
import play.api.libs.ws.Response
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration._
import org.specs2.time.NoTimeConversions
import scala.util.Failure
import scala.concurrent.Awaitable
import play.api.test.Helpers._

object ClientTests extends Specification with NoTimeConversions {

  sequential

  "Client" should {

    "have a health method that returns the health of the server" in {
      val result = awaitResult(testClient.health)

      (result.json \ "cluster_name").as[String] === "elasticsearch"
    }

    "have a create index method" >> {

      "that creates an index" in {

        val result = createTestIndex
        (result.status === OK) and (result.json \ "ok").as[Boolean] === true
      }

      "that throws an exception if an index exists" in {

        val futureResponse = testClient.createIndex(name = testIndexName)

        isException(futureResponse, BAD_REQUEST, testIndexName)
      }
    }

    "have a delete index method" >> {

      "that deletes the index" in {
        val result = deleteTestIndex
        result.status === OK
      }

      "that fails on an unexisting index" in {
        val futureResponse = testClient.deleteIndex(name = testIndexName)

        isException(futureResponse, NOT_FOUND, testIndexName)
      }
    }

    "have an index exists method" in {
      createTestIndex
      existsTestIndex === true
      deleteTestIndex
      existsTestIndex === false
    }
  }

  def createTestIndex = awaitResult(testClient.createIndex(name = testIndexName))
  def deleteTestIndex = awaitResult(testClient.deleteIndex(name = testIndexName))
  def existsTestIndex = awaitResult(testClient.existsIndex(name = testIndexName))

  def isException(futureResponse: Future[Response], status: Int, stringInError: String) = {
    val result = Await.ready(futureResponse, 5.seconds).value
    result must beLike {
      case Some(Failure(Client.ElasticSearchException(status, error))) =>
        error must contain(stringInError)
    }
  }

  def awaitResult[T](t: Awaitable[T]) =
    Await.result(t, 5.seconds)

  val testClient = new Client(url = "http://localhost:9200")
  val testIndexName = "indexname"
}