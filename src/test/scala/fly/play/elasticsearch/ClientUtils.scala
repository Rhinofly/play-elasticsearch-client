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
import java.util.UUID

trait ClientUtils { self: Specification with NoTimeConversions =>

  case class TemporaryIndex(indexName: String, typeName: String)

  val testUrl = "http://localhost:9200"
  val defaultTimeout = 5.seconds

  implicit val ningWSClient = new NingWSClient(new AsyncHttpClientConfig.Builder().build())

  val testClient = new Client(elasticSearchUrl = testUrl)(ningWSClient)

  def testIndex(implicit i: TemporaryIndex) = testClient(indexName = i.indexName)
  def testType(implicit i: TemporaryIndex) = testIndex.apply(typeName = i.typeName)

  def testClientHealth = awaitResult(testClient.health)
  def createTestIndex(implicit i: TemporaryIndex) = awaitResult(testIndex.create(Settings()))
  def createTestIndex(settings: Settings)(implicit i: TemporaryIndex) = awaitResult(testIndex.create(settings))
  def deleteTestIndex(implicit i: TemporaryIndex) = awaitResult(testIndex.delete)
  def existsTestIndex(implicit i: TemporaryIndex) = awaitResult(testIndex.exists)
  def refreshTestIndex(implicit i: TemporaryIndex) = awaitResult(testIndex.refresh)
  def analyze(text: String, analyzer: String)(implicit i: TemporaryIndex) = awaitResult(testIndex.analyze(text, analyzer))

  def index[T: Writes](id: String, doc: T, parameters: Parameter*)(implicit i: TemporaryIndex) = awaitResult(testType.indexV(id = id, doc = doc, parameters: _*))
  def index[T: Writes](doc: T, parameters: Parameter*)(implicit i: TemporaryIndex) = awaitResult(testType.indexV(doc = doc, parameters: _*))
  def getV[T: Reads](id: String, parameters: Parameter*)(implicit i: TemporaryIndex) = awaitResult(testType.getV[T](id = id, parameters: _*))
  def get[T: Reads](id: String, parameters: Parameter*)(implicit i: TemporaryIndex) = awaitResult(testType.get[T](id = id, parameters: _*))
  def del[T](id: String, parameters: Parameter*)(implicit i: TemporaryIndex) = awaitResult(testType.delete(id = id, parameters: _*))
  def update[T: Writes](id: String, doc: T, parameters: Parameter*)(implicit i: TemporaryIndex) = awaitResult(testType.updateV(id = id, doc = doc, parameters: _*))
  def search[T: Reads](query: ElasticSearchQuery, parameters: Parameter*)(implicit i: TemporaryIndex) = awaitResult(testType.search[T](query = query, parameters: _*))

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

  def newTemporaryIndex = TemporaryIndex("testindex_" + UUID.randomUUID.toString, "testtype")

  abstract class WithTempIndex extends Scope {
    implicit lazy val temporaryIndex = newTemporaryIndex

    lazy val TemporaryIndex(testIndexName, testTypeName) = temporaryIndex

  }

  abstract class WithTestIndex extends Scope with Around {

    implicit lazy val temporaryIndex = newTemporaryIndex

    def around[T: AsResult](t: => T): Result = {
      createTestIndex
      try {
        AsResult.effectively(t)
      } finally {
        deleteTestIndex
      }
    }
  }

}
