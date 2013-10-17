package play.modules.elasticsearch

import org.specs2.mutable.Specification
import org.specs2.time.NoTimeConversions
import play.api.libs.json._
import play.modules.elasticsearch.query._

object QueryTests extends Specification with NoTimeConversions {
  
  type WithTestIndex = ClientTests.WithTestIndex
  def put[T: Writes](id: String, doc: T, parameters: Parameter*) = ClientTests.put[T](id = id, doc = doc, parameters: _*)
  def search[T: Reads](query: Query, parameters: Parameter*) = ClientTests.search[T](query = query, parameters : _*)
  def refreshTestIndex = ClientTests.refreshTestIndex

  sequential

  "Query" should {

    br
    
    "when used by the index/type search method" >> {
      
      "find a document using a term-query" in new WithTestIndex {
        val testContent = "test has some content"
        val version = put(id = "test", doc = Json.obj("test" -> testContent), "refresh" -> "true")
        val result = search[JsObject](TermQuery("test", "content"))
        result must beLike {
          case Some(results) =>
            results.hits_total === 1
            results.hits(0).source === Json.obj("test" -> testContent)
        }
      }
      
      "not find documents that do not match the term-query" in new WithTestIndex {
        val testContent = "test has some content"
        val version = put(id = "test", doc = Json.obj("test" -> testContent), "refresh" -> "true")
        val result = search[JsObject](TermQuery("test", "whatever"))
        result must beLike {
          case Some(results) =>
            results.hits_total === 0
            results.hits  === List()
        }
      }
      
      "accept the query-property 'version'" in new WithTestIndex {
        val testContent = "test has some content"
        val version = put(id = "test", doc = Json.obj("test" -> testContent), "refresh" -> "true")
        val result = search[JsObject](TermQuery("test", "content").withVersion(true))
        result must beLike {
          case Some(results) =>
            results.hits_total === 1
            results.hits(0).version == version
            results.hits(0).source === Json.obj("test" -> testContent)
        }
      }
      
      "accept the query-properties 'from' and 'size'" in new WithTestIndex {
        val testContent = "test has some content"
        put(id = "test1", doc = Json.obj("test" -> testContent))
        put(id = "test2", doc = Json.obj("test" -> testContent))
        put(id = "test3", doc = Json.obj("test" -> testContent))
        put(id = "test4", doc = Json.obj("test" -> testContent))
        refreshTestIndex
        val result = search[JsObject](TermQuery("test", "content").withFrom(1).withSize(2))
        result must beLike {
          case Some(results) =>
            results.hits_total === 4
            results.hits.length === 2
            results.hits(0).source === Json.obj("test" -> testContent)
        }
      }
      
      "find documents using a terms-query" in new WithTestIndex {
        put(id = "test1", doc = Json.obj("test" -> "one two three"))
        put(id = "test2", doc = Json.obj("test" -> "one two"))
        put(id = "test3", doc = Json.obj("test" -> "two three"))
        refreshTestIndex
        val result1 = search[JsObject](TermsQuery("test", Seq("one", "two", "three")))
        val result2 = search[JsObject](TermsQuery("test", Seq("one", "two", "three"), 2))
        val result3 = search[JsObject](TermsQuery("test", Seq("one", "two", "three"), 3))
        (result1.get.hits_total === 3) && (result2.get.hits_total === 3) && (result3.get.hits_total === 1)
      }
      
      "find documents using a match-query" in new WithTestIndex {
        put(id = "test1", doc = Json.obj("test" -> "one two three"))
        put(id = "test2", doc = Json.obj("test" -> "one two"))
        put(id = "test3", doc = Json.obj("test" -> "two three"))
        refreshTestIndex
        val result1 = search[JsObject](MatchQuery(field = "test", value = "one two", operator = Operator.or))
        val result2 = search[JsObject](MatchQuery(field = "test", value = "one two", operator = Operator.and))
        val result3 = search[JsObject](MatchQuery(field = "test", value = "one two", matchType = MatchType.phrase))
        val result4 = search[JsObject](MatchQuery(field = "test", value = "one three"))
        val result5 = search[JsObject](MatchQuery(field = "test", value = "one three", matchType = MatchType.phrase))
        val result6 = search[JsObject](MatchQuery(field = "test", value = "one three", matchType = MatchType.phrase, slop = 1))
        (result1.get.hits_total === 3) &&
        (result2.get.hits_total === 2) &&
        (result3.get.hits_total === 2) &&
        (result4.get.hits_total === 3) &&
        (result5.get.hits_total === 0) &&
        (result6.get.hits_total === 1)
      }
      
    }
        
  }

}