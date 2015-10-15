package fly.play.elasticsearch

import scala.concurrent.Await
import scala.concurrent.Awaitable
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.Failure
import org.specs2.execute.AsResult
import org.specs2.execute.Result
import org.specs2.mutable.Around
import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.libs.json.Reads
import play.api.libs.json.Writes
import play.api.libs.json.__
import fly.play.elasticsearch.query.ElasticSearchQuery
import play.api.test.WithApplication
import play.api.test.FakeApplication
import play.api.test.Helpers

trait ClientUtils { self: Specification =>

  val testUrl = "http://localhost:9200"
  val defaultTimeout = 5.seconds

  val testClient = new Client(elasticSearchUrl = testUrl)
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
  def bulk[T: Writes](xs:Seq[(String,T)]) = awaitResult(testType.bulk(xs))

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

  abstract class WithTestIndex extends WithApplication {
    override def around[T: AsResult](t: => T): Result = {
      super.around {
        if (existsTestIndex) deleteTestIndex else ()

        createTestIndex

        AsResult.effectively(t)
      }
    }
  }

}