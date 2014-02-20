package play.modules.elasticsearch

import org.specs2.mutable.Specification
import org.specs2.time.NoTimeConversions
import play.api.libs.json.{JsObject, Json}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.modules.elasticsearch.query._
import play.modules.elasticsearch.query.Query.queryToElasticSearchQuery

object QueryTests extends Specification with NoTimeConversions with ClientUtils {

  sequential

  "Query" should {

    br

    "have a TermQuery sub-class" >> {

      "that finds a matching document" in new WithTestIndex {
        val testContent = "test has some content"
        val version = index(id = "test", doc = Json.obj("test" -> testContent), "refresh" -> "true")
        val result = search[JsObject](TermQuery("test", "content"))
        result.hitsTotal === 1
        result.hits(0).source === Json.obj("test" -> testContent)
      }

      "that does not find non-matching documents" in new WithTestIndex {
        val testContent = "test has some content"
        val version = index(id = "test", doc = Json.obj("test" -> testContent), "refresh" -> "true")
        val result = search[JsObject](TermQuery("test", "whatever"))
        result.hitsTotal === 0
        result.hits === List()
      }

    }

    "have a TermsQuery sub-class" >> {

      "that finds matching documents" in new WithTestIndex {
        index(id = "test1", doc = Json.obj("test" -> "one two three"))
        index(id = "test2", doc = Json.obj("test" -> "one two"))
        index(id = "test3", doc = Json.obj("test" -> "two three"))
        refreshTestIndex
        val result1 = search[JsObject](TermsQuery("test", Seq("one", "two", "three")))
        val result2 = search[JsObject](TermsQuery("test", Seq("one", "two", "three"), 2))
        val result3 = search[JsObject](TermsQuery("test", Seq("one", "two", "three"), 3))
        (result1.hitsTotal === 3) and (result2.hitsTotal === 3) and (result3.hitsTotal === 1)
      }

    }

    "have a MatchQuery sub-class" >> {

      "that finds matching documents" in new WithTestIndex {
        index(id = "test1", doc = Json.obj("test" -> "one two three"))
        index(id = "test2", doc = Json.obj("test" -> "one two"))
        index(id = "test3", doc = Json.obj("test" -> "two three"))
        refreshTestIndex
        val result1 = search[JsObject](MatchQuery(field = "test", value = "one two", operator = Operator.or))
        val result2 = search[JsObject](MatchQuery(field = "test", value = "one two", operator = Operator.and))
        val result3 = search[JsObject](MatchQuery(field = "test", value = "one three"))
        (result1.hitsTotal === 3) and
          (result2.hitsTotal === 2) and
          (result3.hitsTotal === 3)
      }

      "that accepts phrase queries" in new WithTestIndex {
        index(id = "test1", doc = Json.obj("test" -> "one two three"))
        index(id = "test2", doc = Json.obj("test" -> "one two"))
        index(id = "test3", doc = Json.obj("test" -> "two three"))
        refreshTestIndex
        val result1 = search[JsObject](MatchQuery(field = "test", value = "one two", matchType = MatchType.phrase))
        val result2 = search[JsObject](MatchQuery(field = "test", value = "one three", matchType = MatchType.phrase))
        val result3 = search[JsObject](MatchQuery(field = "test", value = "one three", matchType = MatchType.phrase, slop = 1))
        (result1.hitsTotal === 2) and
          (result2.hitsTotal === 0) and
          (result3.hitsTotal === 1)
      }

      "that supports minimum_should_match" in new WithTestIndex {
        index(id = "test1", doc = Json.obj("test" -> "one two three"))
        index(id = "test2", doc = Json.obj("test" -> "one two"))
        index(id = "test3", doc = Json.obj("test" -> "one"))
        refreshTestIndex
        val result1 = search[JsObject](MatchQuery(field = "test", value = "one two three", minimumShouldMatch = "1"))
        val result2 = search[JsObject](MatchQuery(field = "test", value = "one two three", minimumShouldMatch = "2"))
        val result3 = search[JsObject](MatchQuery(field = "test", value = "one two three", minimumShouldMatch = "3"))
        (result1.hitsTotal === 3) and
          (result2.hitsTotal === 2) and
          (result3.hitsTotal === 1)
       }

    }

    "have a MultiMatchQuery sub-class" >> {

      "that finds matching documents" in new WithTestIndex {
        index(id = "test1", doc = Json.obj("keywords" -> "scala play", "description" -> "Play Framework makes it easy to build web applications with Java & Scala."))
        index(id = "test2", doc = Json.obj("keywords" -> "node.js express", "description" -> "Express is a minimal and flexible node.js web application framework."))
        index(id = "test3", doc = Json.obj("keywords" -> "node.js audio", "description" -> "Play sound files from node.js to your speakers."))
        refreshTestIndex
        val result1 = search[JsObject](MultiMatchQuery(fields = Seq("keywords", "description"), value = "play"))
        val result2 = search[JsObject](MultiMatchQuery(fields = Seq("keywords", "description"), value = "play node.js", operator = Operator.and))
        hasHitIds(result1, Set("test1", "test3")) and
          hasHitIds(result2, Set("test3"))
      }

    }

    "have a MatchAllQuery sub-class" >> {

      "that finds no documents if none exist" in new WithTestIndex {
        refreshTestIndex // Otherwise random stuff might hang around in some cache, producing weird errors.
        val result = search[JsObject](MatchAllQuery())
        result.hitsTotal === 0
      }

      "that finds all documents" in new WithTestIndex {
        index(id = "test1", doc = Json.obj("test" -> "one two three"))
        index(id = "test2", doc = Json.obj("test" -> "one two"))
        refreshTestIndex
        val result = search[JsObject](MatchAllQuery())
        result.hitsTotal === 2
      }

    }

    "have a BoolQuery sub-class" >> {

      "that matches documents matching boolean combinations of other queries" in new WithTestIndex {
        index(id = "test1", doc = Json.obj("test" -> "one two three"))
        index(id = "test2", doc = Json.obj("test" -> "one two"))
        refreshTestIndex
        val result = search[JsObject](
          BoolQuery()
            should TermQuery("test", "one")
            must TermQuery("test", "two")
            mustNot TermQuery("test", "three")
        )
        result.hitsTotal === 1
      }

      "that uses 'should' to order the documents" in new WithTestIndex {
        index(id = "test1", doc = Json.obj("test" -> "one two"))
        index(id = "test2", doc = Json.obj("test" -> "one three"))
        refreshTestIndex
        val result1 = search[JsObject](
          BoolQuery()
            should TermQuery("test", "one")
            should TermQuery("test", "two")
        )
        val result2 = search[JsObject](
          BoolQuery()
            should TermQuery("test", "one")
            should TermQuery("test", "three")
        )
        result1.hits.map(_.id) === List("test1", "test2")
        result2.hits.map(_.id) === List("test2", "test1")
      }

      "that supports the minimumShouldMatch parameter" in new WithTestIndex {
        index(id = "test1", doc = Json.obj("test" -> "one two"))
        index(id = "test2", doc = Json.obj("test" -> "one three"))
        index(id = "test3", doc = Json.obj("test" -> "three"))
        refreshTestIndex
        val shoulds = Seq(TermQuery("test", "one"), TermQuery("test", "two"), TermQuery("test", "three"))
        val result1 = search[JsObject](BoolQuery(minimumShouldMatch = "1", shoulds = shoulds))
        val result2 = search[JsObject](BoolQuery(minimumShouldMatch = "2", shoulds = shoulds))
        val result3 = search[JsObject](BoolQuery(minimumShouldMatch = "3", shoulds = shoulds))
        result1.hitsTotal === 3
        result2.hitsTotal === 2
        result3.hitsTotal === 0
      }

    }

    "have a QueryStringQuery sub-class" >> {

      "that finds documents" in new WithTestIndex {
        index(id = "test1", doc = Json.obj("test" -> "one two three"))
        index(id = "test2", doc = Json.obj("test" -> "one two"))
        refreshTestIndex
        val result = search[JsObject](QueryStringQuery("one AND three"))
        result.hitsTotal === 1
      }

    }

  }

  def hasHitIds[T](result: SearchResult[T], ids: Set[Identifier]): Boolean =
    result.hits.map(_.id).toSet === ids

}
