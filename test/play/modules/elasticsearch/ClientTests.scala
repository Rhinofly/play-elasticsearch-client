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
import play.api.libs.json._
import org.specs2.specification.Scope
import org.specs2.mutable.Around
import org.specs2.execute.AsResult
import org.specs2.execute.Result
import org.apache.xalan.xsltc.cmdline.getopt.GetOpt

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
        val result = awaitResult(testClient.health("wait_for_status" -> "green"))
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

      "have an apply method to access a type" in {
        testIndex("test") must beAnInstanceOf[Client#Index#Type]
      }

      "type should" >> {

        "have a put method" >> {
          "to add a document with explicit id to an index and type" in new WithTestIndex {
            val version = put(id = "test", doc = Json.obj("test" -> "test"))
            version === 1
          }

          "to add a document to an index and type" in new WithTestIndex {
            val version = put("test", TestDocument("name"))
            version === 1
          }

          "that accepts parameters" in new WithTestIndex {
            val version =
              put("test", TestDocument("name"),
                "version" -> "2",
                "version_type" -> "external")
            version === 2
          }
        }

        "have a post method" >> {
          "to add a document to an index and type and generate an id" in new WithTestIndex {
            val (version, identifier) = post(doc = Json.obj("test" -> "test"))
            (version === 1) && (identifier !== "")
          }

          "that accepts parameters" in new WithTestIndex {
            val (version, _) =
              post(Json.obj("test" -> "test"),
                "version" -> "2",
                "version_type" -> "external")
            (version === 2)
          }
        }

        "have a get method" >> {

          "to retrieve a document by id from the index and type" in new WithTestIndex {

            val testDocument = TestDocument("name")
            val (version, id) = post(doc = testDocument)
            val optionalTestDocument = get[TestDocument](id = id)
            optionalTestDocument must beLike {
              case Some((v, doc)) =>
                v === version
                doc === testDocument
            }
          }

          "to retrieve nothing for an unexisting id from the index and type" in new WithTestIndex {
            //post(TestDocument("name"))
            val optionalTestDocument = get[TestDocument](id = "non-existing")
            optionalTestDocument === None
          }

          "that accepts parameters" in new WithTestIndex {
            val (_, id) = post(Json.obj("name" -> "name", "test" -> "test"))
            val Some((_, optionalTestDocument1)) = get[JsObject](id, "fields" -> "name")
            optionalTestDocument1 === Json.obj("name" -> "name")
          }
        }
        
        "have a delete method" >> {
          
          "that deletes a document from the index and type" in new WithTestIndex {
            val version = put(id = "test", doc = Json.obj("test" -> "test"))
            val deleted = del("test")
            val optionalTestDocument = get[TestDocument](id = "test")
            (version === 1) && (deleted === true) && (optionalTestDocument === None)
          }
          
          "that accepts parameters" in new WithTestIndex {
            val version = put(id = "test", doc = Json.obj("test" -> "test"))
            val deleted = del("test", "version" -> version.toString)
            val optionalTestDocument = get[TestDocument](id = "test")
            (deleted === true) && (optionalTestDocument === None)            
          }
          
        }
        
        "have an updateDoc method" >> {
          
          "that updates a document in the index and type" in new WithTestIndex {
            val version = put(id = "test", doc = Json.obj("test" -> "test"))
            val nextVersion = updateDoc(id = "test", doc = Json.obj("test" -> "next test"))
            val optionalTestDocument = get[JsObject](id = "test")
            optionalTestDocument must beLike {
              case Some((v, doc)) =>
                v === nextVersion
                doc === Json.obj("test" -> "next test")
            } and (nextVersion === version + 1)
          }
          
          "that can do partial updates" in new WithTestIndex {
            val version = put(id = "test", doc = Json.obj("test" -> "test", "content" -> "content"))
            val nextVersion = updateDoc(id = "test", doc = Json.obj("content" -> "new content"))
            val optionalTestDocument = get[JsObject](id = "test")
            optionalTestDocument must beLike {
              case Some((v, doc)) =>
                v === nextVersion
                doc === Json.obj("test" -> "test", "content" -> "new content")
            }
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

  def post[T: Writes](doc: T, parameters: Parameter*) = awaitResult(testType.post(doc = doc, parameters: _*))
  def get[T: Reads](id: String, parameters: Parameter*) = awaitResult(testType.get[T](id = id, parameters: _*))
  def put[T: Writes](id: String, doc: T, parameters: Parameter*) = awaitResult(testType.put(id = id, doc = doc, parameters: _*))
  def del[T](id: String, parameters: Parameter*) = awaitResult(testType.delete(id = id, parameters: _*))
  def updateDoc[T: Writes](id: String, doc: T, parameters: Parameter*) = awaitResult(testType.updateDoc(id = id, doc = doc, parameters: _*))

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