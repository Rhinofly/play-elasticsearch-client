package fly.play.elasticsearch

import org.specs2.mutable.Specification
import play.api.libs.json.{JsObject, Json, Writes}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.test.Helpers.BAD_REQUEST
import fly.play.elasticsearch.query.{MatchAllQuery, TermQuery}
import play.api.libs.json.JsArray
import play.api.test.WithApplication

object ClientTests extends Specification with ClientUtils {

  sequential

  "Client" should {

    br

    "work without supplying a slash in the url" in new WithApplication {
      testClientHealth must throwA[Throwable].not
    }

    "work with supplying a slash in the url" in new WithApplication {
      awaitResult(new Client(testUrl + "/").health) must throwA[Throwable].not
    }

    "have an apply and index method to access an index" in new WithApplication {
      testClient("test") must beAnInstanceOf[Client#Index]
      testClient.index("test") must beAnInstanceOf[Client#Index]
    }

    "have a health method" >> {
      "that returns the health of the server" in new WithApplication {
        val result = testClientHealth
        (result \ "cluster_name").as[String] !== ""
      }

      "that accepts parameters" in new WithApplication {
        val result = awaitResult(testClient.health("level" -> "indices"))
        (result \ "cluster_name").as[String] !== ""
      }
    }

    "index should" >> {

      "have a create method" >> {

        "that creates an index" in new WithApplication {
          deleteTestIndex
          val result = createTestIndex
          result ===( () )
        }

        "that throws an exception if an index exists" in new WithApplication {
          val futureResponse = testIndex.create
          isException(futureResponse, BAD_REQUEST, testIndexName)
        }

      }

      "have a delete method" >> {

        "that deletes the index" in new WithApplication {
          val result = deleteTestIndex
          result === true
        }

        "that fails on an unexisting index" in new WithApplication {
          val result = deleteTestIndex
          result === false
        }
      }

      "have an exists method" >> {

        "that returns true for an existing index" in new WithApplication {
          createTestIndex
          existsTestIndex === true
        }

        "that returns false for a non-existing index" in new WithApplication {
          deleteTestIndex
          existsTestIndex === false
        }

      }

      "have a refresh method" in new WithApplication {
        createTestIndex
        val result = refreshTestIndex
        deleteTestIndex
        result ===( () )
      }

      "have an analyze method that uses the default analyzer" in new WithApplication {
        createTestIndex
        val result = awaitResult(testIndex.analyze("to be or not to be, that is the question"))
        deleteTestIndex
        result.length !== 0
      }

      "have an analyze method that uses a specific analyzer" in new WithApplication {
        createTestIndex
        val result = analyze("to be or not to be, that is the question", "english")
        deleteTestIndex
        result.length == 1
        result(0).token === "question"
      }

      "have an apply method to access a type" in new WithApplication {
        testIndex("test") must beAnInstanceOf[Client#Index#Type]
      }

      "type should" >> {

        "have an index method" >> {
          "to add a document with explicit id to an index and type" in new WithApplication {
            val version = index(id = "test", doc = Json.obj("test" -> "test"))
            version === 1
          }

          "to add a document to an index and type" in new WithTestIndex {
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

          "that returns Unit" in new WithTestIndex {
            awaitResult(testType.index("test", testDocument)) ===( () )
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

          "that returns an id" in new WithTestIndex {
            val id = awaitResult(testType.index(testDocument))
            id must not beEmpty
          }

          "that throws an exception if a document cannot be jsonified" in {
            case class BadDocument(name: String)
            implicit val badWrites = new Writes[BadDocument] {
              def writes(t: BadDocument) = throw new Exception("bad document")
            }
            awaitResult(testType.index(BadDocument("BAD!"))) must throwA[ElasticSearchException]
          }

        }

        "have a get method" >> {

          "to retrieve a document by id from the index and type" in new WithTestIndex {

            val (id, version) = index(doc = testDocument)
            val optionalTestDocument = getV[TestDocument](id = id)
            optionalTestDocument must beLike {
              case Some((v, doc)) =>
                v === version
                doc === testDocument
            }
          }

          "to retrieve nothing for an unexisting id from the index and type" in new WithTestIndex {
            index("non-existing", testDocument)
            del("non-existing")

            val optionalTestDocument = getV[TestDocument](id = "non-existing")
            optionalTestDocument === None
          }

          "that throws an exception when retrieving from an index that does not exist" in new WithTestIndex {
            if (existsTestIndex) deleteTestIndex else ()
            getV[TestDocument](id = "anything") must throwA[ElasticSearchException]
          }

          "that accepts parameters" in new WithTestIndex {
            val (id, _) = index(Json.obj("name" -> "name", "test" -> "test"))
            refreshTestIndex
            val optionalTestDocument = get[JsObject](id, "fields" -> "name")
            optionalTestDocument === Some(Json.obj("name" -> Seq("name")))
          }

          "that does not return a version" in new WithTestIndex {
            val (id, _) = index(testDocument)
            refreshTestIndex
            val retrievedDocument = get[TestDocument](id)
            retrievedDocument === Some(testDocument)
          }
        }



        "have a delete method" >> {

          "that deletes a document from the index and type" in new WithTestIndex {
            index(id = "test", doc = testDocument)
            refreshTestIndex
            val optionalTestDocument1 = get[TestDocument](id = "test")
            val deleted = del("test")
            refreshTestIndex
            val optionalTestDocument2 = get[TestDocument](id = "test")
            deleted === true
            optionalTestDocument1 === Some(testDocument)
            optionalTestDocument2 === None
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
            val optionalTestDocument = getV[JsObject](id = "test")
            optionalTestDocument must beLike {
              case Some((v, doc)) =>
                v === nextVersion
                doc === Json.obj("test" -> "next test")
            }
          }

          "that can do partial updates" in new WithTestIndex {
            index(id = "test", doc = Json.obj("test" -> "test", "content" -> "content"))
            update(id = "test", doc = Json.obj("content" -> "new content"))
            val optionalTestDocument = getV[JsObject](id = "test")
            optionalTestDocument must beLike {
              case Some((_, doc)) =>
                doc === Json.obj("test" -> "test", "content" -> "new content")
            }
          }

          "that does not return a version" in new WithTestIndex {
            index(id = "test", doc = Json.obj("test" -> "test", "content" -> "content"))
            val result = awaitResult(testType.update(id = "test", doc = Json.obj("content" -> "new content")))
            result ===( () )
          }
        }

        "have a bulk method" >> {
          "that does a bulk insert" in new WithTestIndex {
            bulk(Seq(
              ("1", TestDocument("abc")),
              ("2", TestDocument("xyz"))
            ))

            refreshTestIndex

            get[TestDocument](id = "1") === Some(TestDocument("abc"))
            get[TestDocument](id = "2") === Some(TestDocument("xyz"))
          }

          "that updates existing documents" in new WithTestIndex {
            index(id = "1", doc = TestDocument("old"))

            bulk(Seq(
              ("1", TestDocument("new")),
              ("2", TestDocument("xyz"))
            ))

            refreshTestIndex

            get[TestDocument](id = "1") === Some(TestDocument("new"))
            get[TestDocument](id = "2") === Some(TestDocument("xyz"))
          }
        }


        "have a deleteByQuery method" >> {


          "that deletes documents in a type which match a query" in new WithTestIndex {
            index(id = "test1", doc = Json.obj("test" -> "111"))
            index(id = "test2", doc = Json.obj("test" -> "222"))
            refreshTestIndex
            awaitResult(testType.deleteByQuery(TermQuery("test", "222"))) === true
            search[JsObject](MatchAllQuery()).hitsTotal === 1
          }


          "that deletes all documents in a type with a MatchAllQuery" in new WithTestIndex {
            index(id = "test1", doc = Json.obj("test" -> "111"))
            index(id = "test2", doc = Json.obj("test" -> "222"))
            refreshTestIndex
            awaitResult(testType.deleteByQuery(MatchAllQuery())) === true
            search[JsObject](MatchAllQuery()).hitsTotal === 0
          }

        }

      } // type should


    } // index should


  }


}