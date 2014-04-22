package fly.play.elasticsearch

import org.specs2.execute.{AsResult, Result}
import org.specs2.mutable.{Around, Specification}
import org.specs2.specification.Scope
import org.specs2.time.NoTimeConversions
import play.api.libs.json.{JsObject, Json}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import fly.play.elasticsearch.mapping.{IndexType, ObjectMapping, StoreType, StringMapping, TermVectorType}
import fly.play.elasticsearch.query.{HighlightField, MatchAllQuery, MultiMatchQuery}
import fly.play.elasticsearch.query._
import fly.play.elasticsearch.query.Query.queryToElasticSearchQuery
import fly.play.elasticsearch.query.HighlightType

/**
 * ElasticSearchQuery is an extension of Query, with extra properties.
 * Therefore it has its own tests.
 */
object ElasticSearchQueryTests extends Specification with NoTimeConversions with ClientUtils {

  sequential

  "ElasticSearchQuery" should {

    "accept the query-property 'version'" in new WithTestIndexWithMapping {
      val testContent = "test with some content"
      val version = index(id = "test", doc = Json.obj("content" -> testContent), "refresh" -> "true")
      val result = search[JsObject](TermQuery("content", "content").withVersion(true))
      result.hitsTotal === 1
      result.hits(0).version === Some(version)
      result.hits(0).source === Json.obj("content" -> testContent)
    }

    "accept the query-properties 'from' and 'size'" in new WithTestIndexWithMapping {
      val testContent = "test has some content"
      index(id = "test1", doc = Json.obj("content" -> testContent))
      index(id = "test2", doc = Json.obj("content" -> testContent))
      index(id = "test3", doc = Json.obj("content" -> testContent))
      index(id = "test4", doc = Json.obj("content" -> testContent))
      refreshTestIndex
      val result = search[JsObject](TermQuery("content", "content").withFrom(1).withSize(2))
      result.hitsTotal === 4
      result.hits.length === 2
      result.hits(0).source === Json.obj("content" -> testContent)
    }

    "accept the query-property 'sort'" >> {

      "sort on multiple fields" in new WithTestIndexWithMapping {
        index(id = "test1", doc = Json.obj("test" -> "C", "sub" -> "b"))
        index(id = "test2", doc = Json.obj("test" -> "A"))
        index(id = "test3", doc = Json.obj("test" -> "C", "sub" -> "a"))
        index(id = "test4", doc = Json.obj("test" -> "B", "sub" -> "z"))
        refreshTestIndex
        val result = search[JsObject](MatchAllQuery().withSort(Seq(Sort("test", SortOrder.asc), Sort("sub", SortOrder.asc))))
        result.hits.map(_.id.toString) === Seq("test2", "test4", "test3", "test1")
      }

      "sort in ascending and descending order" in new WithTestIndexWithMapping {
        index(id = "test1", doc = Json.obj("test" -> "C", "sub" -> "b"))
        index(id = "test2", doc = Json.obj("test" -> "A"))
        index(id = "test3", doc = Json.obj("test" -> "C", "sub" -> "a"))
        index(id = "test4", doc = Json.obj("test" -> "B", "sub" -> "z"))
        refreshTestIndex
        val result = search[JsObject](MatchAllQuery().withSort(Seq(Sort("test", SortOrder.desc), Sort("sub", SortOrder.asc))))
        result.hits.map(_.id.toString) === Seq("test3", "test1", "test4", "test2")
      }

      "sort using the 'missing' property" in new WithTestIndexWithMapping {
        index(id = "test1", doc = Json.obj("test" -> "C", "sub" -> "a"))
        index(id = "test2", doc = Json.obj("test" -> "A"))
        index(id = "test3", doc = Json.obj("test" -> "C"))
        index(id = "test4", doc = Json.obj("test" -> "B", "sub" -> "z"))
        refreshTestIndex
        val result = search[JsObject](MatchAllQuery().withSort(Seq(Sort("test", SortOrder.asc), Sort("sub", SortOrder.asc, missing = SortMissing.first))))
        result.hits.map(_.id.toString) === Seq("test2", "test4", "test3", "test1")
        val result2 = search[JsObject](MatchAllQuery().withSort(Seq(Sort("test", SortOrder.asc), Sort("sub", SortOrder.asc, missing = SortMissing.last))))
        result2.hits.map(_.id.toString) === Seq("test2", "test4", "test1", "test3")
      }

    }

    "accept the query-property 'highlight'" >> {

      "show highlights in a result" in new WithTestIndexWithMapping {
        index(id = "test1", doc = Json.obj("content" -> "Rose rose to put rose roes on her rows of roses."))
        refreshTestIndex
        val result = search[JsObject](TermQuery("content", "rose").withHighlight(HighlightField("content")))
        result.hits.length === 1
        result.hits(0).highlightFor("content") must contain("<em>Rose</em> <em>rose</em> to put <em>rose</em> roes on her rows of roses.").forall
      }

      "only show highlights in indicated fields" in new WithTestIndexWithMapping {
        index(id = "test1", doc = Json.obj("content" -> "Truths and roses have thorns about them.", "extra" -> "Rose rose to put rose roes on her rows of roses."))
        refreshTestIndex
        val result = search[JsObject](MultiMatchQuery(fields = Seq("content", "extra"), value = "rows").withHighlight(HighlightField("content")))
        result.hits.length === 1
        result.hits(0).highlightFor("content") === Seq.empty
        result.hits(0).highlight === None
      }

      "show highlights in multiple fields using wildcard" in new WithTestIndexWithMapping {
        index(id = "test1", doc = Json.obj("content" -> "Truths and roses have thorns about them.", "extra" -> "Rose rose to put rose roes on her rows of roses."))
        refreshTestIndex
        val result = search[JsObject](MultiMatchQuery(fields = Seq("content", "extra"), value = "roses").
                       withHighlight(HighlightField("*", highlightType = HighlightType.plain)))
        result.hits.length === 1
        result.hits(0).highlightFor("content") must contain("Truths and <em>roses</em> have thorns about them.")
        result.hits(0).highlightFor("extra") must contain("Rose rose to put rose roes on her rows of <em>roses</em>.")
      }

      "show highlights in multiple fields" in new WithTestIndexWithMapping {
        index(id = "test1", doc = Json.obj("content" -> "Truths and roses have thorns about them.", "extra" -> "Rose rose to put rose roes on her rows of roses."))
        refreshTestIndex
        val result = search[JsObject](MultiMatchQuery(fields = Seq("content", "extra"), value = "roses").withHighlight(HighlightField("content"), HighlightField("extra")))
        result.hits.length === 1
        result.hits(0).highlightFor("content") must contain("Truths and <em>roses</em> have thorns about them.")
        result.hits(0).highlightFor("extra") must contain("Rose rose to put rose roes on her rows of <em>roses</em>.")
      }

      "show highlights using matched_fields" in new WithTestIndexWithMapping {
        index(id = "test1", doc = Json.obj(
            "content" -> "Do not run with scissors because running with scissors is dangerous.",
            "extra"   -> "Do not run with scissors because running with scissors is dangerous."
        ))
        refreshTestIndex
        val result = search[JsObject](QueryStringQuery(query = "running scissors", fields = Seq("extra")).
                       withHighlight(HighlightField("content", highlightType = HighlightType.fvh, matchedFields = Seq("content", "extra"))))
        result.hits.length === 1
        result.hits(0).highlightFor("content") must contain("Do not run with <em>scissors</em> because <em>running</em> with <em>scissors</em> is dangerous.")
      }

      "show highlights in a complete field" in new WithTestIndexWithMapping {
        index(id = "test1", doc = Json.obj(
            "content" -> """In tegenstelling tot wat algemeen aangenomen wordt is Lorem Ipsum niet zomaar willekeurige tekst. het heeft zijn wortels in een stuk klassieke latijnse literatuur uit 45 v.Chr. en is dus meer dan 2000 jaar oud. Richard McClintock, een professor latijn aan de Hampden-Sydney College in Virginia, heeft een van de meer obscure latijnse woorden, consectetur, uit een Lorem Ipsum passage opgezocht, en heeft tijdens het zoeken naar het woord in de klassieke literatuur de onverdachte bron ontdekt. Lorem Ipsum komt uit de secties 1.10.32 en 1.10.33 van "de Finibus Bonorum et Malorum" (De uitersten van goed en kwaad) door Cicero, geschreven in 45 v.Chr. Dit boek is een verhandeling over de theorie der ethiek, erg populair tijdens de renaissance. De eerste regel van Lorem Ipsum, "Lorem ipsum dolor sit amet..", komt uit een zin in sectie 1.10.32."""
        ))
        refreshTestIndex
        val result = search[JsObject](TermQuery("content", "ipsum").
                       withHighlight(HighlightField("content", highlightType = HighlightType.fvh, numberOfFragments = 0)))
        result.hits.length === 1
        result.hits(0).highlightFor("content") must contain("""In tegenstelling tot wat algemeen aangenomen wordt is Lorem <em>Ipsum</em> niet zomaar willekeurige tekst. het heeft zijn wortels in een stuk klassieke latijnse literatuur uit 45 v.Chr. en is dus meer dan 2000 jaar oud. Richard McClintock, een professor latijn aan de Hampden-Sydney College in Virginia, heeft een van de meer obscure latijnse woorden, consectetur, uit een Lorem <em>Ipsum</em> passage opgezocht, en heeft tijdens het zoeken naar het woord in de klassieke literatuur de onverdachte bron ontdekt. Lorem <em>Ipsum</em> komt uit de secties 1.10.32 en 1.10.33 van "de Finibus Bonorum et Malorum" (De uitersten van goed en kwaad) door Cicero, geschreven in 45 v.Chr. Dit boek is een verhandeling over de theorie der ethiek, erg populair tijdens de renaissance. De eerste regel van Lorem <em>Ipsum</em>, "Lorem <em>ipsum</em> dolor sit amet..", komt uit een zin in sectie 1.10.32.""")
      }

      "use tags in highlighted fragments" in new WithTestIndexWithMapping {
        index(id = "test1", doc = Json.obj("content" -> "Rose rose to put rose roes on her rows of roses."))
        refreshTestIndex
        val result = search[JsObject](TermQuery("content", "rose").withHighlight(
            Highlight(fields = Seq(HighlightField("content")), tags = Some(Seq(("""<span class="hilite">""", "</span>"))))
          ))
        result.hits.length === 1
        result.hits(0).highlightFor("content") must contain("""<span class="hilite">Rose</span> <span class="hilite">rose</span> to put <span class="hilite">rose</span> roes on her rows of roses.""").forall
      }

      "use tags_schema in highlighted fragments" in new WithTestIndexWithMapping {
        index(id = "test1", doc = Json.obj("content" -> "Rose rose to put rose roes on her rows of roses."))
        refreshTestIndex
        val result = search[JsObject](TermQuery("content", "rose").withHighlight(
            Highlight(fields = Seq(HighlightField("content")), tagsSchema = Some("styled"))
          ))
        result.hits.length === 1
        result.hits(0).highlightFor("content") must contain("""<em class="hlt1">Rose</em> <em class="hlt1">rose</em> to put <em class="hlt1">rose</em> roes on her rows of roses.""").forall
      }

      // Encoder does not seem to work as documented, so we do not test it.

      "use fragment_size and number_of_fragments" in new WithTestIndexWithMapping {
        index(id = "test1", doc = Json.obj("content" -> "Rose rose to put rose roes on her rows of roses. Do not run with scissors. Truths and roses have thorns about them."))
        refreshTestIndex
        val result = search[JsObject](TermQuery("content", "roses").withHighlight(
            Highlight(fields = Seq(HighlightField("content", fragmentSize = 18, numberOfFragments = 2)))
          ))
        result.hits.length === 1
        result.hits(0).highlightFor("content") must contain("rows of <em>roses</em>. Do not", "Truths and <em>roses</em> have thorns")
      }

      "use highlight_query" in new WithTestIndexWithMapping {
        index(id = "test1", doc = Json.obj("content" -> "Rose rose to put rose roes on her rows of roses. Do not run with scissors."))
        refreshTestIndex
        val result = search[JsObject](TermQuery("content", "rose").withHighlight(
            Highlight(fields = Seq(HighlightField("content", highlightQuery = Some(TermQuery("content", "scissors")))))
          ))
        result.hits.length === 1
        result.hits(0).highlightFor("content") must contain("rose roes on her rows of roses. Do not run with <em>scissors</em>.")
      }

      "use global settings" in new WithTestIndexWithMapping {
        index(id = "test1", doc = Json.obj("content" -> "Rose rose to put rose roes on her rows of roses. Do not run with scissors."))
        refreshTestIndex
        val result = search[JsObject](TermQuery("content", "rose").withHighlight(
            Highlight(fields = Seq(HighlightField("content")), tagsSchema = Some("styled"), highlightQuery = Some(TermQuery("content", "scissors")))
          ))
        result.hits.length === 1
        result.hits(0).highlightFor("content") must contain("""rose roes on her rows of roses. Do not run with <em class="hlt1">scissors</em>.""")
      }

      "use require_field_match" in new WithTestIndexWithMapping {
        index(id = "test1", doc = Json.obj(
            "content" -> "Rose rose to put rose roes on her rows of roses. Do not run with scissors.",
            "extra" -> "Rose rose to put rose roes on her rows of roses."
          ))
        refreshTestIndex
        val result1 = search[JsObject](QueryStringQuery(fields = Seq("content"), query = "roses AND scissors").withHighlight(
            HighlightField("extra")
          ))
        result1.hits.length === 1
        result1.hits(0).highlightFor("extra") must contain("""Rose rose to put rose roes on her rows of <em>roses</em>.""")
        val result2 = search[JsObject](QueryStringQuery(fields = Seq("content"), query = "roses AND scissors").withHighlight(
            HighlightField("extra", requireFieldMatch = true)
          ))
        result2.hits.length === 1
        result2.hits(0).highlightFor("extra") === Seq.empty
      }

      // Boundary characters does not seem to work as documented, so we do not test it.

    }

  }

  /* Don't analyze test and sub, which would remove the "a" stopword. */
  val testMapping =
    ObjectMapping(testTypeName, properties = Set(
      StringMapping("content", store = StoreType.yes, index = IndexType.analyzed, termVector = TermVectorType.with_positions_offsets),
      StringMapping("extra",   store = StoreType.no,  index = IndexType.analyzed, termVector = TermVectorType.with_positions_offsets),
      StringMapping("test",    store = StoreType.yes, index = IndexType.not_analyzed),
      StringMapping("sub",     store = StoreType.yes, index = IndexType.not_analyzed)
    ))

  abstract class WithTestIndexWithMapping extends Scope with Around {
    def around[T: AsResult](t: => T): Result = {
      if (existsTestIndex) deleteTestIndex else true
      awaitResult(testIndex.create(Settings(), Seq(testMapping)))
      AsResult(t)
    }
  }

}
