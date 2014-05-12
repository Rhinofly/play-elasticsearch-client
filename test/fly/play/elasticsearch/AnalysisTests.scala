package fly.play.elasticsearch.analysis

import org.specs2.execute.{AsResult, Result}
import org.specs2.mutable.{Around, Specification}
import org.specs2.specification.Scope
import org.specs2.time.NoTimeConversions
import play.api.libs.json.{Format, JsObject, Json, JsPath, JsSuccess, Reads, Writes}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import fly.play.elasticsearch.{ClientUtils, Settings}
import fly.play.elasticsearch.utils.JsonUtils
import fly.play.elasticsearch.mapping.{IndexType, Mapping, NestableMapping, ObjectMapping, StoreType, StringMapping}
import fly.play.elasticsearch.query.{MatchQuery, MultiMatchQuery, TermQuery}

object AnalysisTests extends Specification with NoTimeConversions with ClientUtils {

  abstract class WithTestIndexWithAnalysis(analysis: Analysis, mapping: NestableMapping) extends Scope with Around {
    def around[T: AsResult](t: => T): Result = {
      if (existsTestIndex) deleteTestIndex
      awaitResult( testIndex.create(Settings(analysis = Some(analysis)), Seq(ObjectMapping(testTypeName, properties = Set(mapping)))) )
      try {
        AsResult.effectively(t)
      }
      // Leave the testIndex for inspection.
    }
  }

  abstract class WithTestIndexWithSimpleAnalysis(analysis: Analysis) extends Scope with Around {
    def around[T: AsResult](t: => T): Result = {
      if (existsTestIndex) deleteTestIndex
      createTestIndex(Settings(analysis = Some(analysis)))
      try {
        AsResult.effectively(t)
      }
      // Leave the testIndex for inspection.
    }
  }

  sequential

  "Analysis should" >> {

    /* Analyzer */

    "have a WhitespaceAnalyzer class that divides text at whitespace" in
        new WithTestIndexWithSimpleAnalysis(Analysis(Seq(WhitespaceAnalyzer("analyzer")))) {
      val result = analyze("to be or not to be", "analyzer")
      result must containTheSameElementsAs(Seq("to", "be", "or", "not", "to", "be"))
    }

    "have a StopAnalyzer class that skips stopwords" in
        new WithTestIndexWithSimpleAnalysis(Analysis(Seq(StopAnalyzer("analyzer", stopwords = Some(Seq("to", "be")))))) {
      val result = analyze("to be or not to be", "analyzer")
      result must containTheSameElementsAs(Seq("or", "not"))
    }

    "have a KeywordAnalyzer class that tokenizes an entire stream as a single token" in
        new WithTestIndexWithSimpleAnalysis(Analysis(Seq(KeywordAnalyzer("analyzer")))) {
      val result = analyze("to be or not to be", "analyzer")
      result must containTheSameElementsAs(Seq("to be or not to be"))
    }

    "have a PatternAnalyzer class that can flexibly separate text into terms via a regular expression" in
        new WithTestIndexWithSimpleAnalysis(Analysis(Seq( PatternAnalyzer("analyzer", pattern = Some(""",\s*""")) ))) {
      val result = analyze("to be, or not to be", "analyzer")
      result must containTheSameElementsAs(Seq("to be", "or not to be"))
    }

    "have a LanguageAnalyzer class that analyzes specific language text" in
        new WithTestIndexWithSimpleAnalysis(Analysis(Seq(
          LanguageAnalyzer("en", language = "english"),
          LanguageAnalyzer("nl", language = "dutch")
        ))) {
      val enResult = analyze("to be or not to be, that is the question", "english")
      enResult must containTheSameElementsAs(Seq("question"))
      val nlResult = analyze("to be or not to be, that is the question", "english")
      nlResult must containTheSameElementsAs(Seq("to", "be", "or", "not", "to", "be", "that", "is", "the", "question"))
    }

    "have a SnowballAnalyzer class that uses a snowball filter" in
        new WithTestIndexWithSimpleAnalysis(Analysis(Seq(
          SnowballAnalyzer("snowball2", language = Some("English"), stopwords = Some(Seq("or", "be")))
        ))) {
      val result = analyze("to be or not to be", "analyzer")
      result must containTheSameElementsAs(Seq("to", "not", "to"))
    }

    "have a CustomAnalyzer class that allows you to compose your own analyzer" in
        new WithTestIndexWithSimpleAnalysis(Analysis(
          analyzers = Seq( CustomAnalyzer("analyzer", tokenizer = "standard", filter = Some(Seq()), charFilter = Some(Seq("html"))) ),
          charFilters = Seq( HtmlStripCharFilter("html") )
        )) {
      val result = analyze("to be or <em>not</em> to be", "analyzer")
      result must containTheSameElementsAs(Seq("to", "be", "or", "not", "to", "be"))
    }

    /* Tokenizer */

    "have a StandardTokenizer class that" >> {

      val analysis = Analysis(
        analyzers = Seq(CustomAnalyzer("test-analyzer", tokenizer = "test-tokenizer")),
        tokenizers = Seq(StandardTokenizer("test-tokenizer", maxTokenLength = 3))
      )
      val mapping = StringMapping("test-field", analyzer = "test-analyzer")

      "is used for indexing and querying" in new WithTestIndexWithAnalysis(analysis, mapping) {
        index(id = "1", doc = Json.obj("test-field" -> "one two three four"), "refresh" -> "true")
        val result1 = search[JsObject](TermQuery("test-field", "one"))
        val result2 = search[JsObject](TermQuery("test-field", "four"))
        result1.hitsTotal === 1
        result2.hitsTotal === 0
      }

    }

    /* TokenFilter */

    "have a AsciiFoldingTokenFilter class that" >> {

      val filter = AsciiFoldingTokenFilter("AsciiFoldingTokenFilter")

      val analysis = Analysis(
        analyzers = Seq(CustomAnalyzer("testanalyzer", tokenizer = "letter", filter = Some(Seq("asciifolding")))),
        filters = Seq(AsciiFoldingTokenFilter("asciifolding"))
      )
      val mapping = StringMapping("testfield", analyzer = "testanalyzer", store = StoreType.yes)
      "is used for indexing and querying" in new WithTestIndexWithAnalysis(analysis, mapping) {
        val sentence = "\u00AB Conna\u00EEtre le pass\u00E9 est une mani\u00E8re de s\'en lib\u00E9rer. \u00BB"
        index(id = "1", doc = Json.obj("testfield" -> sentence, "notanalyzed" -> sentence), "refresh" -> "true")
        val result1 = search[JsObject](MatchQuery("testfield", "Connaitre"))
        val result2 = search[JsObject](MatchQuery("testfield", "connaitre"))
        val result3 = search[JsObject](MultiMatchQuery(fields = Seq("testfield", "notanalyzed"), value = "Connaitre"))
        val result4 = search[JsObject](MatchQuery("notanalyzed", "Connaitre"))
        result1.hitsTotal === 1
        result2.hitsTotal === 0
        result3.hitsTotal === 1
        result4.hitsTotal === 0
      }

    }

    "have a SynonymTokenFilter class that" >> {

      val analysis = Analysis(
        analyzers = Seq(CustomAnalyzer("test-analyzer", tokenizer = "standard", filter = Some(Seq("synonym")))),
        filters = Seq(SynonymTokenFilter("synonym", synonyms = Some(Seq("mini, small, little, slight", "big, great, tall"))))
      )
      val mapping = StringMapping("test-field", analyzer = "test-analyzer")
      "is used for indexing and querying" in new WithTestIndexWithAnalysis(analysis, mapping) {
        index(id = "1", doc = Json.obj("test-field" -> "This is a small sentence.", "not-analyzed" -> "This is a small sentence."), "refresh" -> "true")
        val result0 = search[JsObject](TermQuery("test-field", "tiny"))
        val result1 = search[JsObject](TermQuery("test-field", "small"))
        val result2 = search[JsObject](TermQuery("test-field", "little"))
        val result3 = search[JsObject](MultiMatchQuery(fields = Seq("test-field", "not-analyzed"), value = "little"))
        val result4 = search[JsObject](TermQuery("not-analyzed", "small"))
        val result5 = search[JsObject](TermQuery("not-analyzed", "little"))
        result0.hitsTotal === 0
        result1.hitsTotal === 1
        result2.hitsTotal === 1
        result3.hitsTotal === 1
        result4.hitsTotal === 1
        result5.hitsTotal === 0
      }

    }

    /* CharFilter */

    /* A more complicated example to test a real-world case. */
    // It looks like the query is not properly analyzed.

    "be capable of dealing with Dutch texts" >> {
      val nlTokenFilters = Seq("ascii_folding", "lowercase", "nl_synonyms", "nl_stemmer")
      val analysis = Analysis(
        analyzers = Seq(
          CustomAnalyzer("nl_text",
            tokenizer = "letter",
            filter = Some(nlTokenFilters)),
          CustomAnalyzer("nl_html",
            tokenizer = "letter",
            filter = Some(nlTokenFilters),
            charFilter = Some(Seq("html")))),
        tokenizers = Seq(),
        filters = Seq(
          AsciiFoldingTokenFilter("ascii_folding"),
          LowercaseTokenFilter("lowercase"),
          StemmerTokenFilter("nl_stemmer", language = "dutch"),
          SynonymTokenFilter("nl_synonyms", synonyms = Some(Seq("bijzonder, speciaal", "fiets, rijwiel")))),
        charFilters = Seq(
          HtmlStripCharFilter("html")))
      "for indexing and querying text" in new WithTestIndexWithAnalysis(analysis, StringMapping("test-field", analyzer = "nl_text")) {
        index(id = "1", doc = Json.obj("test-field" -> "Mijn bijzondere fiets is gestolen."), "refresh" -> "true")
        val result1 = search[JsObject](MatchQuery("test-field", "Fiets"))
        val result2 = search[JsObject](MatchQuery("test-field", "rijwiel"))
        val result3 = search[JsObject](MatchQuery("test-field", "speciaal"))
        result1.hitsTotal === 1
        result2.hitsTotal === 1
        result3.hitsTotal === 1
      }
      "for indexing and querying html" in new WithTestIndexWithAnalysis(analysis, StringMapping("test-field", analyzer = "nl_html")) {
        index(id = "1", doc = Json.obj("test-field" -> "Mijn <em>bijzondere</em> fiets is <span>gestolen</span>."), "refresh" -> "true")
        val result1 = search[JsObject](MatchQuery("test-field", "Fiets"))
        val result2 = search[JsObject](MatchQuery("test-field", "rijwiel"))
        val result3 = search[JsObject](MatchQuery("test-field", "speciaal"))
        val result4 = search[JsObject](MatchQuery("test-field", "span"))
        result1.hitsTotal === 1
        result2.hitsTotal === 1
        result3.hitsTotal === 1
        result4.hitsTotal === 0
      }
    }

  } // "Analysis should"

}
