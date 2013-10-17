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
    
    "be used by the search method" >> {    
          
      "that accepts the query-property 'version'" in new WithTestIndex {
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
      
      "that accepts the query-properties 'from' and 'size'" in new WithTestIndex {
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
      
    }

    br
    
    "have a TermQuery sub-class" >> {
      
      "that finds a matching document" in new WithTestIndex {
        val testContent = "test has some content"
        val version = put(id = "test", doc = Json.obj("test" -> testContent), "refresh" -> "true")
        val result = search[JsObject](TermQuery("test", "content"))
        result must beLike {
          case Some(results) =>
            results.hits_total === 1
            results.hits(0).source === Json.obj("test" -> testContent)
        }
      }
      
      "that does not find non-matching documents" in new WithTestIndex {
        val testContent = "test has some content"
        val version = put(id = "test", doc = Json.obj("test" -> testContent), "refresh" -> "true")
        val result = search[JsObject](TermQuery("test", "whatever"))
        result must beLike {
          case Some(results) =>
            results.hits_total === 0
            results.hits  === List()
        }
      }
      
    }
    
    "have a TermsQuery sub-class" >> {
      
      "that finds matching documents" in new WithTestIndex {
        put(id = "test1", doc = Json.obj("test" -> "one two three"))
        put(id = "test2", doc = Json.obj("test" -> "one two"))
        put(id = "test3", doc = Json.obj("test" -> "two three"))
        refreshTestIndex
        val result1 = search[JsObject](TermsQuery("test", Seq("one", "two", "three")))
        val result2 = search[JsObject](TermsQuery("test", Seq("one", "two", "three"), 2))
        val result3 = search[JsObject](TermsQuery("test", Seq("one", "two", "three"), 3))
        (result1.get.hits_total === 3) && (result2.get.hits_total === 3) && (result3.get.hits_total === 1)
      }
      
    }
    
    "have a MatchQuery sub-class" >> {
      
      "that finds matching documents" in new WithTestIndex {
        put(id = "test1", doc = Json.obj("test" -> "one two three"))
        put(id = "test2", doc = Json.obj("test" -> "one two"))
        put(id = "test3", doc = Json.obj("test" -> "two three"))
        refreshTestIndex
        val result1 = search[JsObject](MatchQuery(field = "test", value = "one two", operator = Operator.or))
        val result2 = search[JsObject](MatchQuery(field = "test", value = "one two", operator = Operator.and))
        val result3 = search[JsObject](MatchQuery(field = "test", value = "one three"))
        (result1.get.hits_total === 3) &&
        (result2.get.hits_total === 2) &&
        (result3.get.hits_total === 3)
      }
      
      "that accepts phrase queries" in new WithTestIndex {
        put(id = "test1", doc = Json.obj("test" -> "one two three"))
        put(id = "test2", doc = Json.obj("test" -> "one two"))
        put(id = "test3", doc = Json.obj("test" -> "two three"))
        refreshTestIndex
        val result1 = search[JsObject](MatchQuery(field = "test", value = "one two", matchType = MatchType.phrase))
        val result2 = search[JsObject](MatchQuery(field = "test", value = "one three", matchType = MatchType.phrase))
        val result3 = search[JsObject](MatchQuery(field = "test", value = "one three", matchType = MatchType.phrase, slop = 1))
        (result1.get.hits_total === 2) &&
        (result2.get.hits_total === 0) &&
        (result3.get.hits_total === 1)
      }
      
    }
    
    "have a MultiMatchQuery sub-class" >> {
      
      "that finds matching documents" in new WithTestIndex {
        put(id = "test1", doc = Json.obj("keywords" -> "scala play", "description" -> "Play Framework makes it easy to build web applications with Java & Scala."))
        put(id = "test2", doc = Json.obj("keywords" -> "node.js express", "description" -> "Express is a minimal and flexible node.js web application framework."))
        put(id = "test3", doc = Json.obj("keywords" -> "node.js audio", "description" -> "Play sound files from node.js to your speakers."))
        refreshTestIndex
        val result1 = search[JsObject](MultiMatchQuery(fields = Seq("keywords", "description"), value = "play"))
        val result2 = search[JsObject](MultiMatchQuery(fields = Seq("keywords", "description"), value = "play node.js", operator = Operator.and))
        hasHitIds(result1.get, Set("test1", "test3")) &&
        hasHitIds(result2.get, Set("test3"))
      }
      
    }
    
    "have a MatchAllQuery sub-class" >> {
      
      "that finds all documents" in new WithTestIndex {
        put(id = "test1", doc = Json.obj("test" -> "one two three"))
        put(id = "test2", doc = Json.obj("test" -> "one two"))
        refreshTestIndex
        val result = search[JsObject](MatchAllQuery())
        result.get.hits_total === 2
      }
      
    }

  }
  
  def hasHitIds[T](result: SearchResult[T], ids: Set[Identifier]) : Boolean =
    result.hits.map(_.id).toSet === ids

}