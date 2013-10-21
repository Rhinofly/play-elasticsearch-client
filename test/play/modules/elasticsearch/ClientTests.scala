package play.modules.elasticsearch

import org.apache.xalan.xsltc.cmdline.getopt.GetOpt
import org.specs2.execute.AsResult
import org.specs2.execute.Result
import org.specs2.mutable.Around
import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import org.specs2.time.NoTimeConversions
import scala.concurrent.Await
import scala.concurrent.Awaitable
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Failure
import play.api.libs.json._
import play.api.libs.ws.Response
import play.api.test.Helpers._
import play.modules.elasticsearch.query._

object ClientTests extends Specification with NoTimeConversions {

  val testUrl = "http://localhost:9200"

  sequential

  "Client" should {

    br

    "work without supplying a slash in the url" in {
      testClientHealth must throwA[Throwable].not
    }

    "work with supplying a slash in the url" in {
      awaitResult(new Client(testUrl + "/").health) must throwA[Throwable].not
    }

    "have an apply and index method to access an index" in {
      testClient("test") must beAnInstanceOf[Client#Index]
      testClient.index("test") must beAnInstanceOf[Client#Index]
    }

    br

    "have a health method" >> {
      "that returns the health of the server" in {
        val result = testClientHealth
        (result \ "cluster_name").as[String] === "elasticsearch"
      }

      "that accepts parameters" in {
        val result = awaitResult(testClient.health("level" -> "indices"))
        (result \ "cluster_name").as[String] === "elasticsearch"
      }
    }

    br

    "index should" >> {

      "have a create method" >> {

        "that creates an index" in {
          val result = createTestIndex
          result === ()
        }

        "that throws an exception if an index exists" in {
          val futureResponse = testIndex.create
          isException(futureResponse, BAD_REQUEST, testIndexName)
        }
      }

      "have a delete method" >> {

        "that deletes the index" in {
          val result = deleteTestIndex
          result === ()
        }

        "that fails on an unexisting index" in {
          val futureResponse = testIndex.delete
          isException(futureResponse, NOT_FOUND, testIndexName)
        }
      }

      "have an exists method" >> {

        "that returns true for an existing index" in {
          createTestIndex
          existsTestIndex === true
        }

        "that returns false for a non-existing index" in {
          deleteTestIndex
          existsTestIndex === false
        }

      }

      "have a refresh method" in {
        createTestIndex
        val result = refreshTestIndex
        deleteTestIndex
        result === ()
      }

      "have an apply method to access a type" in {
        testIndex("test") must beAnInstanceOf[Client#Index#Type]
      }

      "type should" >> {

        "have a put method" >> {
          "to add a document with explicit id to an index and type" in new WithTestIndex {
            val version = put(id = "test", doc = Json.obj("test" -> "test"))
            version === 1
          }

          "to add a class to an index and type" in new WithTestIndex {
            val version = put("test", testDocument)
            version === 1
          }

          "that accepts parameters" in new WithTestIndex {
            val version =
              put("test", testDocument,
                "version" -> "2",
                "version_type" -> "external")
            version === 2
          }

          "that does not return a version" in new WithTestIndex {
            awaitResult(testType.put("test", testDocument)) === ()
          }
        }

        "have a post method" >> {
          "to add a document to an index and type and generate an id" in new WithTestIndex {
            val (identifier, version) = post(doc = Json.obj("test" -> "test"))
            (version === 1) and (identifier must not beEmpty)
          }

          "that accepts parameters" in new WithTestIndex {
            val (_, version) =
              post(Json.obj("test" -> "test"),
                "version" -> "2",
                "version_type" -> "external")
            (version === 2)
          }

          "that does not return a version" in new WithTestIndex {
            val id = awaitResult(testType.post(testDocument))
            id must not beEmpty
          }
        }

        "have a get method" >> {

          "to retrieve a document by id from the index and type" in new WithTestIndex {

            val (id, version) = post(doc = testDocument)
            val optionalTestDocument = get[TestDocument](id = id)
            optionalTestDocument must beLike {
              case Some((v, doc)) =>
                v === version
                doc === testDocument
            }
          }

          "to retrieve nothing for an unexisting id from the index and type" in new WithTestIndex {
            put("non-existing", testDocument)
            del("non-existing")

            val optionalTestDocument = get[TestDocument](id = "non-existing")
            optionalTestDocument === None
          }

          "that accepts parameters" in new WithTestIndex {
            val (id, _) = post(Json.obj("name" -> "name", "test" -> "test"))
            val Some((_, optionalTestDocument)) = get[JsObject](id, "fields" -> "name")

            optionalTestDocument === Json.obj("name" -> "name")
          }

          "that does not return a version" in new WithTestIndex {
            val (id, _) = post(testDocument)
            val retrievedDocument = awaitResult(testType.get[TestDocument](id))
            retrievedDocument === Some(testDocument)
          }
        }

        "have a delete method" >> {

          "that deletes a document from the index and type" in new WithTestIndex {
            put(id = "test", doc = Json.obj("test" -> "test"))
            val deleted = del("test")
            val optionalTestDocument = get[TestDocument](id = "test")
            (deleted === true) and (optionalTestDocument === None)
          }

          "that does not delete a non existing document from the index and type" in new WithTestIndex {
            val deleted = del("non-existing")
            (deleted === false)
          }

          "that accepts parameters" in new WithTestIndex {
            val firstVersion = put(id = "test", doc = Json.obj("test" -> "test"))
            put(id = "test", doc = Json.obj("test" -> "test"))
            del("test", "version" -> firstVersion.toString) must throwAn[ElasticSearchException].like {
              case ElasticSearchException(409, _, _) => ok
            }
          }
        }

        "have an merge method" >> {

          "that merges a document in the index and type" in new WithTestIndex {
            put(id = "test", doc = Json.obj("test" -> "test"))
            val nextVersion = merge(id = "test", doc = Json.obj("test" -> "next test"))
            val optionalTestDocument = get[JsObject](id = "test")
            optionalTestDocument must beLike {
              case Some((v, doc)) =>
                v === nextVersion
                doc === Json.obj("test" -> "next test")
            }
          }

          "that can do partial updates" in new WithTestIndex {
            put(id = "test", doc = Json.obj("test" -> "test", "content" -> "content"))
            merge(id = "test", doc = Json.obj("content" -> "new content"))
            val optionalTestDocument = get[JsObject](id = "test")
            optionalTestDocument must beLike {
              case Some((_, doc)) =>
                doc === Json.obj("test" -> "test", "content" -> "new content")
            }
          }
          
          "that does not return a version" in new WithTestIndex {
            put(id = "test", doc = Json.obj("test" -> "test", "content" -> "content"))
            val result = awaitResult(testType.merge(id = "test", doc = Json.obj("content" -> "new content")))
            result === ()
          }
        }
      }
    }
  }

  val defaultTimeout = 5.seconds

  val testClient = new Client(elasticSearchUrl = testUrl)
  val testIndexName = "indexname"
  val testTypeName = "typename"

  def testIndex = testClient(indexName = testIndexName)
  def testType = testIndex(typeName = testTypeName)

  def testClientHealth = awaitResult(testClient.health)
  def createTestIndex = awaitResult(testIndex.create)
  def deleteTestIndex = awaitResult(testIndex.delete)
  def existsTestIndex = awaitResult(testIndex.exists)
  def refreshTestIndex = awaitResult(testIndex.refresh)

  def post[T: Writes](doc: T, parameters: Parameter*) = awaitResult(testType.postV(doc = doc, parameters: _*))
  def get[T: Reads](id: String, parameters: Parameter*) = awaitResult(testType.getV[T](id = id, parameters: _*))
  def put[T: Writes](id: String, doc: T, parameters: Parameter*) = awaitResult(testType.putV(id = id, doc = doc, parameters: _*))
  def del[T](id: String, parameters: Parameter*) = awaitResult(testType.delete(id = id, parameters: _*))
  def merge[T: Writes](id: String, doc: T, parameters: Parameter*) = awaitResult(testType.mergeV(id = id, doc = doc, parameters: _*))
  def search[T: Reads](query: Query, parameters: Parameter*) = awaitResult(testType.search[T](query = query, parameters: _*))

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
      createTestIndex
      try {
        AsResult(t)
      } finally {
        deleteTestIndex
      }
    }
  }

}