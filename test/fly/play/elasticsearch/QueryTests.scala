package fly.play.elasticsearch

import org.specs2.mutable.Specification
import org.specs2.time.NoTimeConversions
import play.api.libs.json.{JsObject, Json}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import fly.play.elasticsearch.query._
import fly.play.elasticsearch.query.Query.queryToElasticSearchQuery

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
        result1.hits.map(_.id) must containTheSameElementsAs(Seq("test1", "test3"))
        result2.hits.map(_.id) must containTheSameElementsAs(Seq("test3"))
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
        val query = BoolQuery() should TermQuery("test", "one") must TermQuery("test", "two") mustNot TermQuery("test", "three")
        val result = search[JsObject](query)
        result.hitsTotal === 1
      }

      "that uses 'should' to order the documents" in new WithTestIndex {
        index(id = "test1", doc = Json.obj("test" -> "one two"))
        index(id = "test2", doc = Json.obj("test" -> "one three"))
        refreshTestIndex
        val query = BoolQuery() should TermQuery("test", "one") should TermQuery("test", "two")
        val result1 = search[JsObject](query)
        val result2 = search[JsObject](
          BoolQuery() should TermQuery("test", "one") should TermQuery("test", "three")
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

    "have a DisMaxQuery sub-class" >> {

      "that finds documents" in new WithTestIndex {
        index(id = "test1", doc = Json.obj("test" -> "one two"))
        index(id = "test2", doc = Json.obj("test" -> "three four"))
        index(id = "test3", doc = Json.obj("test" -> "five six"))
        refreshTestIndex
        val result = search[JsObject](DisMaxQuery(queries = Seq(TermQuery("test", "one"), TermQuery("test", "three"))))
        result.hitsTotal === 2
      }

    }

    "have a BoostingQuery sub-class" >> {

      "that finds documents" in new WithTestIndex {
        index(id = "test1", doc = Json.obj("test" -> "one two"))
        index(id = "test2", doc = Json.obj("test" -> "two three"))
        index(id = "test3", doc = Json.obj("test" -> "three four"))
        refreshTestIndex
        val result = search[JsObject](BoostingQuery(positive = QueryStringQuery("one OR three"), negative = TermQuery("test", "three"), negativeBoost = 0.5))
        (result.hits(0).source \ "test").as[String] === "one two"
      }

    }

    "have a FuzzyLikeThisQuery sub-class" >> {

      "that does not find documents when searching for an exact match" in new WithTestIndex {
        index(id = "test1", doc = Json.obj("test" -> "one two"))
        index(id = "test2", doc = Json.obj("test" -> "two three"))
        index(id = "test3", doc = Json.obj("test" -> "three four"))
        refreshTestIndex
        val resultExact = search[JsObject](FuzzyLikeThisQuery(fields = Seq("test"), likeText = "tree", fuzziness = "0"))
        resultExact.hitsTotal === 0
      }

      "that finds documents with a slightly different search term when searching for a fuzzy match" in new WithTestIndex {
        index(id = "test1", doc = Json.obj("test" -> "one two"))
        index(id = "test2", doc = Json.obj("test" -> "two three"))
        index(id = "test3", doc = Json.obj("test" -> "three four"))
        refreshTestIndex
        val resultFuzzy = search[JsObject](FuzzyLikeThisQuery(fields = Seq("test"), likeText = "tree", fuzziness = "0.5"))
        resultFuzzy.hitsTotal === 2
      }

      "accepts all parameters" in new WithTestIndex {
        index(id = "test1", doc = Json.obj("test" -> "Fuzzy like this query find documents that are like provided text by running it against one or more fields."))
        index(id = "test2", doc = Json.obj("test" -> "Fuzzifies ALL terms provided as strings and then picks the best n differentiating terms. In effect this mixes the behaviour of FuzzyQuery and MoreLikeThis but with special consideration of fuzzy scoring factors."))
        index(id = "test3", doc = Json.obj("test" -> "This generally produces good results for queries where users may provide details in a number of fields and have no knowledge of boolean query syntax and also want a degree of fuzzy matching and a fast query."))
        refreshTestIndex
        val resultFuzzy = search[JsObject](FuzzyLikeThisQuery(
          fields = Seq("test"), likeText = "furry",
          ignoreTf = true, maxQueryTerms = 10,
          fuzziness = "0.2", prefixLength = 2, boost = 2.0, analyzer = "simple"
        ))
        resultFuzzy.hitsTotal === 3
      }

    }

    " have a FuzzyLikeThisFieldQuery sub-class" >> {

      "that does not find documents when searching for an exact match" in new WithTestIndex {
        index(id = "test1", doc = Json.obj("test" -> "one two"))
        index(id = "test2", doc = Json.obj("test" -> "two three"))
        index(id = "test3", doc = Json.obj("test" -> "three four"))
        refreshTestIndex
        val resultExact = search[JsObject](FuzzyLikeThisFieldQuery(field = "test", likeText = "tree", fuzziness = "0"))
        resultExact.hitsTotal === 0
      }

      "that finds documents with a slightly different search term when searching for a fuzzy match" in new WithTestIndex {
        index(id = "test1", doc = Json.obj("test" -> "one two"))
        index(id = "test2", doc = Json.obj("test" -> "two three"))
        index(id = "test3", doc = Json.obj("test" -> "three four"))
        refreshTestIndex
        val resultFuzzy = search[JsObject](FuzzyLikeThisFieldQuery(field = "test", likeText = "tree", fuzziness = "0.5"))
        resultFuzzy.hitsTotal === 2
      }

      "accepts all parameters" in new WithTestIndex {
        index(id = "test1", doc = Json.obj("test" -> "Fuzzy like this query find documents that are like provided text by running it against one or more fields."))
        index(id = "test2", doc = Json.obj("test" -> "Fuzzifies ALL terms provided as strings and then picks the best n differentiating terms. In effect this mixes the behaviour of FuzzyQuery and MoreLikeThis but with special consideration of fuzzy scoring factors."))
        index(id = "test3", doc = Json.obj("test" -> "This generally produces good results for queries where users may provide details in a number of fields and have no knowledge of boolean query syntax and also want a degree of fuzzy matching and a fast query."))
        refreshTestIndex
        val resultFuzzy = search[JsObject](FuzzyLikeThisFieldQuery(
          field = "test", likeText = "furry",
          ignoreTf = true, maxQueryTerms = 10,
          fuzziness = "0.2", prefixLength = 2, boost = 2.0, analyzer = "simple"
        ))
        resultFuzzy.hitsTotal === 3
      }

    }

    " have a FuzzyQuery sub-class" >> {

      "that does not find documents when searching for an exact match" in new WithTestIndex {
        index(id = "test1", doc = Json.obj("test" -> "The fuzzy query uses similarity based on Levenshtein edit distance for string fields, and a +/- margin on numeric and date fields."))
        index(id = "test2", doc = Json.obj("test" -> "The fuzzy query generates all possible matching terms that are within the maximum edit distance specified in fuzziness."))
        index(id = "test3", doc = Json.obj("test" -> "It then checks the term dictionary to find out which of those generated terms actually exist in the index."))
        refreshTestIndex
        val resultExact = search[JsObject](FuzzyQuery(field = "test", value = "funny", fuzziness = "0"))
        resultExact.hitsTotal === 0
      }

      "that finds documents with a slightly different search term when searching for a fuzzy match" in new WithTestIndex {
        index(id = "test1", doc = Json.obj("test" -> "The fuzzy query uses similarity based on Levenshtein edit distance for string fields, and a +/- margin on numeric and date fields."))
        index(id = "test2", doc = Json.obj("test" -> "The fuzzy query generates all possible matching terms that are within the maximum edit distance specified in fuzziness."))
        index(id = "test3", doc = Json.obj("test" -> "It then checks the term dictionary to find out which of those generated terms actually exist in the index."))
        refreshTestIndex
        val resultFuzzy = search[JsObject](FuzzyQuery(field = "test", value = "funniness"))
        resultFuzzy.hits.map(_.id) must containTheSameElementsAs(Seq("test2"))
      }

    }

    "have a IdsQuery sub-class" >> {

      "that finds documents" in new WithTestIndex {
        index(id = "test1", doc = Json.obj("test" -> "one two"))
        index(id = "test2", doc = Json.obj("test" -> "two three"))
        index(id = "test3", doc = Json.obj("test" -> "three four"))
        refreshTestIndex
        val result = search[JsObject](IdsQuery(values = Seq("test2", "test3")))
        result.hits.map(_.id) must containTheSameElementsAs(Seq("test2", "test3"))
      }

    }

    "have a MoreLikeThisQuery sub-class" >> {

      "that finds documents with a text like this" in new WithTestIndex {
        val likeThis = "Do you like this short piece of text?"
        index(id = "test1", doc = Json.obj("test" -> "This is a short piece of text which you may like."))
        index(id = "test2", doc = Json.obj("test" -> "However, this has nothing to do with it."))
        index(id = "test3", doc = Json.obj("test" -> "This piece of text is more like it, don't you think?"))
        refreshTestIndex
        val result = search[JsObject](MoreLikeThisQuery(fields = Seq("test"), likeText = likeThis, percentTermsToMatch = 0.5, minTermFreq = 1, minDocFreq = 1))
        result.hits.map(_.id) must containTheSameElementsAs(Seq("test1", "test3"))
      }

    }

    "have a MoreLikeThisFieldQuery sub-class" >> {

      "that finds documents with a text like this" in new WithTestIndex {
        val likeThis = "Do you like this short piece of text?"
        index(id = "test1", doc = Json.obj("test" -> "This is a short piece of text which you may like."))
        index(id = "test2", doc = Json.obj("test" -> "However, this has nothing to do with it."))
        index(id = "test3", doc = Json.obj("test" -> "This piece of text is more like it, don't you think?"))
        refreshTestIndex
        val result = search[JsObject](MoreLikeThisFieldQuery(field = "test", likeText = likeThis, percentTermsToMatch = 0.5, minTermFreq = 1, minDocFreq = 1))
        result.hits.map(_.id) must containTheSameElementsAs(Seq("test1", "test3"))
      }

    }

    "have a RangeQuery sub-class" >> {

      "that finds documents using a string range" in new WithTestIndex {
        index(id = "test1", doc = Json.obj("text" -> "all before cee"))
        index(id = "test2", doc = Json.obj("text" -> "some words after dee"))
        index(id = "test3", doc = Json.obj("text" -> "only stuff past o up to u"))
        refreshTestIndex
        val result1 = search[JsObject](RangeQuery("text", from = "n", to = "v"))
        result1.hits.map(_.id) must containTheSameElementsAs(Seq("test2", "test3"))
        val result2 = search[JsObject](RangeQuery("text", from = "d", to = "o"))
        result2.hits.map(_.id) must containTheSameElementsAs(Seq("test2", "test3"))
        val result3 = search[JsObject](RangeQuery("text", from = "d", to = "o", includeUpper = false))
        result3.hits.map(_.id) must containTheSameElementsAs(Seq("test2"))
        val result4 = search[JsObject](RangeQuery("text", from = "cee", to = "zebras", includeLower = false))
        result4.hits.map(_.id) must containTheSameElementsAs(Seq("test2", "test3"))
      }

    }

  }

}
