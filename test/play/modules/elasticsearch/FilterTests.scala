package play.modules.elasticsearch

import org.specs2.mutable.Specification
import org.specs2.time.NoTimeConversions
import play.api.libs.json.{JsObject, Json}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.modules.elasticsearch.filter.{ExistsFilter, RangeExecution, RangeFilter, TermFilter}
import play.modules.elasticsearch.query.{FilteredQuery, TermQuery}
import play.modules.elasticsearch.query.Query.queryToElasticSearchQuery
import play.modules.elasticsearch.filter.MissingFilter
import play.api.libs.json.JsNull
import play.modules.elasticsearch.filter.AndFilter
import play.modules.elasticsearch.filter.OrFilter
import play.modules.elasticsearch.filter.NotFilter
import play.modules.elasticsearch.filter.BoolFilter

object FilterTests extends Specification with NoTimeConversions with ClientUtils {

  sequential

  "Filter" should {

    br

    "be used in a FilteredQuery, combined with a query" in new WithTestIndex {
      index(id = "test11", doc = Json.obj("content" -> "something", "year" -> 2011))
      index(id = "test12", doc = Json.obj("content" -> "something", "year" -> 2012))
      index(id = "test13", doc = Json.obj("content" -> "nothing", "year" -> 2013))
      refreshTestIndex
      val result = search[JsObject](FilteredQuery(query = TermQuery("content", "something"), filter = RangeFilter("year", from = 2012, to = 2014)))
      result.hitsTotal === 1
      hasHitIds(result, Set("test12"))
    }

    br

    "have a TermFilter sub-class" >> {

      "that finds a matching document" in new WithTestIndex {
        index(id = "test", doc = Json.obj("test" -> "test has some content"))
        refreshTestIndex
        val result = search[JsObject](FilteredQuery(filter = TermFilter("test", "content")))
        result.hitsTotal === 1
        hasHitIds(result, Set("test"))
      }

      "that does not find non-matching documents" in new WithTestIndex {
        index(id = "test", doc = Json.obj("test" -> "test has some content"))
        refreshTestIndex
        val result = search[JsObject](FilteredQuery(filter = TermFilter("test", "whatever")))
        result.hitsTotal === 0
        result.hits === List()
      }

    }

    "have a RangeFilter sub-class" >> {

      "that finds matching documents with a numeric field" in new WithTestIndex {
        index(id = "test1", doc = Json.obj("year" -> 1963))
        index(id = "test2", doc = Json.obj("year" -> 1993))
        index(id = "test3", doc = Json.obj("year" -> 2013))
        refreshTestIndex
        val result = search[JsObject](FilteredQuery(filter = RangeFilter(field = "year", from = 1990, to = 2000)))
        result.hitsTotal === 1
        hasHitIds(result, Set("test2"))
      }

      "that finds matching documents with a numeric field, excluding lower bound" in new WithTestIndex {
        index(id = "test1", doc = Json.obj("year" -> 1963))
        index(id = "test2", doc = Json.obj("year" -> 1993))
        index(id = "test3", doc = Json.obj("year" -> 2013))
        refreshTestIndex
        val result = search[JsObject](FilteredQuery(filter = RangeFilter(field = "year", from = 1993, to = 2000, includeLower = false)))
        result.hitsTotal === 0
      }

      "that finds matching documents with a numeric field, excluding upper bound" in new WithTestIndex {
        index(id = "test1", doc = Json.obj("year" -> 1963))
        index(id = "test2", doc = Json.obj("year" -> 1993))
        index(id = "test3", doc = Json.obj("year" -> 2013))
        refreshTestIndex
        val result = search[JsObject](FilteredQuery(filter = RangeFilter(field = "year", from = 1990, to = 1993, includeUpper = false)))
        result.hitsTotal === 0
      }

      "that finds a matching document with a string field" in new WithTestIndex {
        index(id = "test1", doc = Json.obj("name" -> "alice"))
        index(id = "test2", doc = Json.obj("name" -> "bob"))
        index(id = "test3", doc = Json.obj("name" -> "carl"))
        refreshTestIndex
        val result = search[JsObject](FilteredQuery(filter = RangeFilter(field = "name", from = "b", to = "c")))
        result.hitsTotal === 1
        hasHitIds(result, Set("test2"))
      }

      "that finds multiple matching documents with a string field" in new WithTestIndex {
        index(id = "test1", doc = Json.obj("name" -> "alice"))
        index(id = "test2", doc = Json.obj("name" -> "bob"))
        index(id = "test3", doc = Json.obj("name" -> "carl"))
        refreshTestIndex
        val result = search[JsObject](FilteredQuery(filter = RangeFilter(field = "name", from = "a", to = "c")))
        result.hitsTotal === 2
        hasHitIds(result, Set("test1", "test2"))
      }

      "that has an execution option" in new WithTestIndex {
        index(id = "test1", doc = Json.obj("year" -> 1963))
        index(id = "test2", doc = Json.obj("year" -> 1993))
        index(id = "test3", doc = Json.obj("year" -> 2013))
        refreshTestIndex
        val result = search[JsObject](FilteredQuery(filter = RangeFilter(field = "year", from = 1990, to = 2000, execution = RangeExecution.fielddata)))
        result.hitsTotal === 1
        hasHitIds(result, Set("test2"))
      }

    }

    "have a ExistsFilter sub-class" >> {

      "that finds matching documents" in new WithTestIndex {
        index(id = "test1", doc = Json.obj("title" -> "Programming In Scala"))
        index(id = "test2", doc = Json.obj("title" -> "Play for Scala", "year" -> 2013))
        refreshTestIndex
        val result = search[JsObject](FilteredQuery(filter = ExistsFilter(field = "year")))
        result.hitsTotal === 1
        hasHitIds(result, Set("test2"))
      }

    }

    "have a MissingFilter sub-class" >> {

      "that finds matching documents" in new WithTestIndex {
        index(id = "test1", doc = Json.obj("year" -> JsNull))
        index(id = "test2", doc = Json.obj("year" -> 2013))
        refreshTestIndex
        val result = search[JsObject](FilteredQuery(filter = MissingFilter(field = "year", nullValue = true)))
        result.hitsTotal === 1
        hasHitIds(result, Set("test1"))
      }

    }

    "have a AndFilter sub-class" >> {

      "that finds matching documents" in new WithTestIndex {
        index(id = "2012", doc = Json.obj("year" -> 2012))
        index(id = "2013", doc = Json.obj("year" -> 2013))
        index(id = "2014", doc = Json.obj("year" -> 2014))
        refreshTestIndex
        val result = search[JsObject](FilteredQuery(filter = AndFilter(Seq(
            RangeFilter(field = "year", from = 0, to = 2013),
            RangeFilter(field = "year", from = 2013, to = 9999)
          ))))
        result.hitsTotal === 1
        hasHitIds(result, Set("2013"))
      }

    }

    "have a OrFilter sub-class" >> {

      "that finds matching documents" in new WithTestIndex {
        index(id = "2012", doc = Json.obj("year" -> 2012))
        index(id = "2013", doc = Json.obj("year" -> 2013))
        index(id = "2014", doc = Json.obj("year" -> 2014))
        refreshTestIndex
        val result = search[JsObject](FilteredQuery(filter = OrFilter(Seq(
            RangeFilter(field = "year", from = 0, to = 2013),
            RangeFilter(field = "year", from = 2013, to = 9999)
          ))))
        result.hitsTotal === 3
        hasHitIds(result, Set("2012", "2013", "2014"))
      }

      "that finds matching documents within ranges" in new WithTestIndex {
        index(id = "2012", doc = Json.obj("year" -> 2012))
        index(id = "2013", doc = Json.obj("year" -> 2013))
        index(id = "2014", doc = Json.obj("year" -> 2014))
        refreshTestIndex
        val result = search[JsObject](FilteredQuery(filter = OrFilter(Seq(
            RangeFilter(field = "year", from = 0, to = 2013, includeUpper = false),
            RangeFilter(field = "year", from = 2013, to = 9999, includeLower = false)
          ))))
        result.hitsTotal === 2
        hasHitIds(result, Set("2012", "2014"))
      }

    }

    "have a NotFilter sub-class" >> {

      "that finds documents not matching a filter" in new WithTestIndex {
        index(id = "2012", doc = Json.obj("year" -> 2012))
        index(id = "2013", doc = Json.obj("year" -> 2013))
        index(id = "2014", doc = Json.obj("year" -> 2014))
        refreshTestIndex
        val result = search[JsObject](FilteredQuery(filter = NotFilter(
            RangeFilter(field = "year", from = 2013, to = 2013)
          )))
        result.hitsTotal === 2
        hasHitIds(result, Set("2012", "2014"))
      }

    }

    "have a BoolFilter sub-class" >> {

      "that matches documents matching boolean combinations of other filters" in new WithTestIndex {
        index(id = "test1", doc = Json.obj("test" -> "one two three"))
        index(id = "test2", doc = Json.obj("test" -> "one two"))
        refreshTestIndex
        val result = search[JsObject](FilteredQuery(filter =
          BoolFilter()
            should TermFilter("test", "one")
            must TermFilter("test", "two")
            mustNot TermFilter("test", "three")
        ))
        result.hitsTotal === 1
      }

      "that uses 'should' and 'must'" in new WithTestIndex {
        index(id = "test1", doc = Json.obj("test" -> "one two"))
        index(id = "test2", doc = Json.obj("test" -> "one three"))
        refreshTestIndex
        val result1 = search[JsObject](FilteredQuery(filter =
          BoolFilter()
            should TermFilter("test", "one")
            should TermFilter("test", "two")
        ))
        val result2 = search[JsObject](FilteredQuery(filter =
          BoolFilter()
            must TermFilter("test", "one")
            must TermFilter("test", "two")
        ))
        result1.hitsTotal === 2
        result2.hitsTotal === 1
      }

    }

  }

  def hasHitIds[T](result: SearchResult[T], ids: Set[Identifier]): Boolean =
    result.hits.map(_.id).toSet === ids

}
