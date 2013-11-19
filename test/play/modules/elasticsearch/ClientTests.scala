package play.modules.elasticsearch

import org.specs2.mutable.Specification
import org.specs2.time.NoTimeConversions

import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.test.Helpers.BAD_REQUEST

object ClientTests extends Specification with NoTimeConversions with ClientUtils {

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
        (result \ "cluster_name").as[String] !== ""
      }

      "that accepts parameters" in {
        val result = awaitResult(testClient.health("level" -> "indices"))
        (result \ "cluster_name").as[String] !== ""
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
          result === true
        }

        "that fails on an unexisting index" in {
          val result = deleteTestIndex
          result === false
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

        "have an index method" >> {
          "to add a document with explicit id to an index and type" in new WithTestIndex {
            val version = index(id = "test", doc = Json.obj("test" -> "test"))
            version === 1
          }

          "to add a class to an index and type" in new WithTestIndex {
            val version = index("test", testDocument)
            version === 1
          }

          "that accepts parameters" in new WithTestIndex {
            val version =
              index("test", testDocument,
                "version" -> "2",
                "version_type" -> "external")
            version === 2
          }

          "that does not return a version" in new WithTestIndex {
            awaitResult(testType.index("test", testDocument)) === ()
          }
        }

        "have an index method" >> {
          "to add a document to an index and type and generate an id" in new WithTestIndex {
            val (identifier, version) = index(doc = Json.obj("test" -> "test"))
            (version === 1) and (identifier must not beEmpty)
          }

          "that accepts parameters" in new WithTestIndex {
            val (_, version) =
              index(Json.obj("test" -> "test"),
                "version" -> "2",
                "version_type" -> "external")
            (version === 2)
          }

          "that does not return a version" in new WithTestIndex {
            val id = awaitResult(testType.index(testDocument))
            id must not beEmpty
          }
        }

        "have a get method" >> {

          "to retrieve a document by id from the index and type" in new WithTestIndex {

            val (id, version) = index(doc = testDocument)
            val optionalTestDocument = get[TestDocument](id = id)
            optionalTestDocument must beLike {
              case Some((v, doc)) =>
                v === version
                doc === testDocument
            }
          }

          "to retrieve nothing for an unexisting id from the index and type" in new WithTestIndex {
            index("non-existing", testDocument)
            del("non-existing")

            val optionalTestDocument = get[TestDocument](id = "non-existing")
            optionalTestDocument === None
          }

          "that throws an exception when retrieving from an index that does not exist"  in {
            get[TestDocument](id = "anything") must throwA[ElasticSearchException]
          }

          "that accepts parameters" in new WithTestIndex {
            val (id, _) = index(Json.obj("name" -> "name", "test" -> "test"))
            val Some((_, optionalTestDocument)) = get[JsObject](id, "fields" -> "name")

            optionalTestDocument === Json.obj("name" -> "name")
          }

          "that does not return a version" in new WithTestIndex {
            val (id, _) = index(testDocument)
            val retrievedDocument = awaitResult(testType.get[TestDocument](id))
            retrievedDocument === Some(testDocument)
          }
        }

        "have a delete method" >> {

          "that deletes a document from the index and type" in new WithTestIndex {
            index(id = "test", doc = Json.obj("test" -> "test"))
            val deleted = del("test")
            val optionalTestDocument = get[TestDocument](id = "test")
            (deleted === true) and (optionalTestDocument === None)
          }

          "that does not delete a non existing document from the index and type" in new WithTestIndex {
            val deleted = del("non-existing")
            (deleted === false)
          }

          "that accepts parameters" in new WithTestIndex {
            val firstVersion = index(id = "test", doc = Json.obj("test" -> "test"))
            index(id = "test", doc = Json.obj("test" -> "test"))
            del("test", "version" -> firstVersion.toString) must throwAn[ElasticSearchException].like {
              case ElasticSearchException(409, _, _) => ok
            }
          }
        }

        "have an update method" >> {

          "that updates a document in the index and type" in new WithTestIndex {
            index(id = "test", doc = Json.obj("test" -> "test"))
            val nextVersion = update(id = "test", doc = Json.obj("test" -> "next test"))
            val optionalTestDocument = get[JsObject](id = "test")
            optionalTestDocument must beLike {
              case Some((v, doc)) =>
                v === nextVersion
                doc === Json.obj("test" -> "next test")
            }
          }

          "that can do partial updates" in new WithTestIndex {
            index(id = "test", doc = Json.obj("test" -> "test", "content" -> "content"))
            update(id = "test", doc = Json.obj("content" -> "new content"))
            val optionalTestDocument = get[JsObject](id = "test")
            optionalTestDocument must beLike {
              case Some((_, doc)) =>
                doc === Json.obj("test" -> "test", "content" -> "new content")
            }
          }

          "that does not return a version" in new WithTestIndex {
            index(id = "test", doc = Json.obj("test" -> "test", "content" -> "content"))
            val result = awaitResult(testType.update(id = "test", doc = Json.obj("content" -> "new content")))
            result === ()
          }
        }
      }
    }
  }


}