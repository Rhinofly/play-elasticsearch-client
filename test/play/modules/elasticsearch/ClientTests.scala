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
import play.api.libs.json.JsObject
import play.api.libs.json.Json

object ClientTests extends Specification with NoTimeConversions {

  sequential

  "Client" should {

    br

    "have a health method that returns the health of the server" in {
      val result = awaitResult(testClient.health)

      (result.json \ "cluster_name").as[String] === "elasticsearch"
    }

    br

    "have a create index method" >> {

      "that creates an index" in {

        val result = createTestIndex
        (result.status === OK) and (result.json \ "ok").as[Boolean] === true
      }

      "that throws an exception if an index exists" in {

        val futureResponse = testIndex.create

        isException(futureResponse, BAD_REQUEST, testIndexName)
      }
    }

    "have a delete index method" >> {

      "that deletes the index" in {
        val result = deleteTestIndex
        result.status === OK
      }

      "that fails on an unexisting index" in {
        val futureResponse = testIndex.delete

        isException(futureResponse, NOT_FOUND, testIndexName)
      }
    }

    "have an index exists method" >> {

      "that returns true for an existing index" in {
        createTestIndex
        existsTestIndex === true
      }

      "that returns false for a non-existing index" in {
        deleteTestIndex
        existsTestIndex === false
      }
    }

    "have a put method to add a document to an index and type" in {
      testIndex(typeName = testTypeName).put(doc = Json.obj(
        "test" -> Json.obj(
          "aasdasd" -> Json.arr(
            "adasd"))))
      todo
    }
  }

  def testIndex = testClient(indexName = testIndexName)
  def createTestIndex = awaitResult(testIndex.create)
  def deleteTestIndex = awaitResult(testIndex.delete)
  def existsTestIndex = awaitResult(testIndex.exists)

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
  val testTypeName = "typename"
}