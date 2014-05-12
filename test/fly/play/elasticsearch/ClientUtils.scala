package fly.play.elasticsearch

import com.ning.http.client.AsyncHttpClientConfig
import fly.play.elasticsearch.query.ElasticSearchQuery
import org.specs2.execute.{ AsResult, Result }
import org.specs2.mutable.{ Around, Specification }
import org.specs2.specification.Scope
import org.specs2.time.NoTimeConversions
import play.api.libs.json.{ JsValue, Json }
import play.api.libs.json.{ Reads, Writes, __ }
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.libs.ws.ning.NingWSClient
import scala.concurrent.{ Await, Awaitable, Future }
import scala.concurrent.duration.DurationInt
import scala.util.Failure

trait ClientUtils { self: Specification with NoTimeConversions =>

  val testUrl = "http://localhost:9200"
  val defaultTimeout = 5.seconds

  implicit val ningWSClient = new NingWSClient(new AsyncHttpClientConfig.Builder().build())
  val testClient = new Client(elasticSearchUrl = testUrl)(ningWSClient)
  val testIndexName = "indexname"
  val testTypeName = "typename"

  def testIndex = testClient(indexName = testIndexName)
  def testType = testIndex(typeName = testTypeName)

  def testClientHealth = awaitResult(testClient.health)
  def createTestIndex = awaitResult(testIndex.create(Settings()))
  def createTestIndex(settings: Settings) = awaitResult(testIndex.create(settings))
  def deleteTestIndex = awaitResult(testIndex.delete)
  def existsTestIndex = awaitResult(testIndex.exists)
  def refreshTestIndex = awaitResult(testIndex.refresh)
  def analyze(text: String, analyzer: String) = awaitResult(testIndex.analyze(text, analyzer))

  def index[T: Writes](id: String, doc: T, parameters: Parameter*) = awaitResult(testType.indexV(id = id, doc = doc, parameters: _*))
  def index[T: Writes](doc: T, parameters: Parameter*) = awaitResult(testType.indexV(doc = doc, parameters: _*))
  def getV[T: Reads](id: String, parameters: Parameter*) = awaitResult(testType.getV[T](id = id, parameters: _*))
  def get[T: Reads](id: String, parameters: Parameter*) = awaitResult(testType.get[T](id = id, parameters: _*))
  def del[T](id: String, parameters: Parameter*) = awaitResult(testType.delete(id = id, parameters: _*))
  def update[T: Writes](id: String, doc: T, parameters: Parameter*) = awaitResult(testType.updateV(id = id, doc = doc, parameters: _*))
  def search[T: Reads](query: ElasticSearchQuery, parameters: Parameter*) = awaitResult(testType.search[T](query = query, parameters: _*))

  val testDocument = TestDocument("name")

  case class TestDocument(name: String)
  implicit val testWrites =
    new Writes[TestDocument] {
      def writes(t: TestDocument): JsValue = Json.obj("test" -> t.name)
    }
  implicit val testReads: Reads[TestDocument] =
    (__ \ 'test).read[String].map(TestDocument.apply _)

  def awaitResult[T](t: Awaitable[T]) =
    Await.result(t, defaultTimeout)

  def isException(futureResponse: Future[_], expectedStatus: Int, stringInError: String) = {
    val result = Await.ready(futureResponse, defaultTimeout).value
    result must beLike {
      case Some(Failure(ElasticSearchException(status, error, _))) =>
        status === expectedStatus
        error must contain(stringInError)
    }
  }

  abstract class WithTestIndex extends Scope with Around {
    def around[T: AsResult](t: => T): Result = {
      if (existsTestIndex) deleteTestIndex else ()
      createTestIndex
      AsResult(t)
      /* Leave the testIndex after the last test, for inspection via ES-head. */
    }
  }

}
