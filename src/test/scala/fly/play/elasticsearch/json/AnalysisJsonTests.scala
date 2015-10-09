package fly.play.elasticsearch.analysis

import org.specs2.execute.{AsResult, Result}
import org.specs2.mutable.{Around, Specification}
import org.specs2.specification.Scope
import play.api.libs.json.{Format, JsObject, Json, JsPath, JsSuccess, Reads, Writes}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import fly.play.elasticsearch.{ClientUtils, Settings}
import fly.play.elasticsearch.utils.JsonUtils
import fly.play.elasticsearch.mapping.{IndexType, Mapping, NestableMapping, ObjectMapping, StoreType, StringMapping}
import fly.play.elasticsearch.query.{MatchQuery, MultiMatchQuery, TermQuery}

object AnalysisJsonTests extends Specification with ClientUtils {

  /**
   * Test implicit Format[T] both ways (reads / writes).
   */
  def jsonFormatTest[T: Format](t: T, j: JsObject) = {
    Json.toJson(t) === j
    Json.fromJson[T](j).asOpt === Some(t) // Turning this into Option[T] throws away the path from the JsResult.
  }

  /**
   * Test round-trip for list-formats.
   * Lists of analyzers / tokenizers / tokenfilters / charfilters are used in JSON definition objects of the form
   *   {"name1": {<definition1>}, "name2": {<definition2>} ... }
   * as expected by ElasticSearch.
   * See Analyzer.analyzerListReads and Analyzer.analyzerListWrites.
   * Here we test sequences of one component by writing it to JSON, then reading it.
   */
  def analyzerSeqTest(t: Analyzer) = {
    Analyzer.analyzerListReads.reads(Analyzer.analyzerListWrites.writes(Seq(t))) === JsSuccess(Seq(t))
  }

  def tokenizerSeqTest(t: Tokenizer) = {
    Tokenizer.tokenizerListReads.reads(Tokenizer.tokenizerListWrites.writes(Seq(t))) === JsSuccess(Seq(t))
  }

  def tokenFilterSeqTest(f: TokenFilter) = {
    TokenFilter.filterListReads.reads(TokenFilter.filterListWrites.writes(Seq(f))) === JsSuccess(Seq(f))
  }

  def charFilterSeqTest(f: CharFilter) = {
    CharFilter.filterListReads.reads(CharFilter.filterListWrites.writes(Seq(f))) === JsSuccess(Seq(f))
  }

  sequential

  "Analysis should" >> {

    /* Analyzer */

    "have a StandardAnalyzer class that" >> {

      val standardAnalyzer1 = StandardAnalyzer(name = "standard1")
      val standardJson1 = Json.obj("name" -> "standard1", "max_token_length" -> StandardAnalyzer.defaultMaxTokenLength)

      val standardAnalyzer2 = StandardAnalyzer(name = "standard2", stopwords = Some(Seq("stop", "words")), maxTokenLength = 42)
      val standardJson2 = Json.obj("name" -> "standard2", "stopwords" -> Some(Seq("stop", "words")), "max_token_length" -> 42)

      "has a JSON representation" in {
        jsonFormatTest(standardAnalyzer1, standardJson1)
        jsonFormatTest(standardAnalyzer2, standardJson2)
      }

      "is recognized when part of a definition object" in {
        analyzerSeqTest(standardAnalyzer1)
      }

    }

    "have a SimpleAnalyzer class that" >> {

      val analyzer = SimpleAnalyzer("simple")

      "has a JSON representation" in {
        jsonFormatTest(analyzer, Json.obj("name" -> "simple"))
      }

      "is recognized when part of a definition object" in {
        analyzerSeqTest(analyzer)
      }

    }

    "have a WhitespaceAnalyzer class that" >> {

      val analyzer = WhitespaceAnalyzer("WhitespaceAnalyzer")

      "has a JSON representation" in {
        jsonFormatTest(analyzer, Json.obj("name" -> "WhitespaceAnalyzer"))
      }

      "is recognized when part of a definition object" in {
        analyzerSeqTest(analyzer)
      }

    }

    "have a StopAnalyzer class that" >> {

      val stopAnalyzer1 = StopAnalyzer("stop1")
      val stopJson1 = Json.obj("name" -> "stop1")

      val stopAnalyzer2 = StopAnalyzer("stop2", stopwords = Some(Seq("stop", "words")))
      val stopJson2 = Json.obj("name" -> "stop2", "stopwords" -> Some(Seq("stop", "words")))

      val stopAnalyzer3 = StopAnalyzer("stop3", stopwordsPath = Some("path/to/stopwords"))
      val stopJson3 = Json.obj("name" -> "stop3", "stopwords_path" -> Some("path/to/stopwords"))

      "has a JSON representation" in {
        jsonFormatTest(stopAnalyzer1, stopJson1)
        jsonFormatTest(stopAnalyzer2, stopJson2)
        jsonFormatTest(stopAnalyzer3, stopJson3)
      }

      "is recognized when part of a definition object" in {
        analyzerSeqTest(stopAnalyzer1)
      }

    }

    "have a KeywordAnalyzer class that" >> {

      val analyzer = KeywordAnalyzer("KeywordAnalyzer")

      "has a JSON representation" in {
        jsonFormatTest(analyzer, Json.obj("name" -> "KeywordAnalyzer"))
      }

      "is recognized when part of a definition object" in {
        analyzerSeqTest(analyzer)
      }

    }

    "have a PatternAnalyzer class that" >> {

      val patternAnalyzer1 = PatternAnalyzer("pattern1")
      val patternJson1 = Json.obj("name" -> "pattern1", "lowercase" -> PatternAnalyzer.defaultLowercase)

      val patternAnalyzer2 = PatternAnalyzer("pattern2", pattern = Some(""",\s*"""), flags = Some("CASE_INSENSITIVE|COMMENTS"), lowercase = false)
      val patternJson2 = Json.obj("name" -> "pattern2", "pattern" -> Some(""",\s*"""), "flags" -> Some("CASE_INSENSITIVE|COMMENTS"), "lowercase" -> false)

      "has a JSON representation" in {
        jsonFormatTest(patternAnalyzer1, patternJson1)
        jsonFormatTest(patternAnalyzer2, patternJson2)
      }

      "is recognized when part of a definition object" in {
        analyzerSeqTest(patternAnalyzer1)
      }

    }

    "have a LanguageAnalyzer class that" >> {

      val languageAnalyzer1 = LanguageAnalyzer("language1", language = "english")
      val languageJson1 = Json.obj("name" -> "language1", "type" -> "english")

      val languageAnalyzer2 = LanguageAnalyzer("language2", language = "dutch", stopwords = Some(Seq("de", "het", "een")), stemExclusion = Some(Seq("fiets")))
      val languageJson2 = Json.obj("name" -> "language2", "type" -> "dutch", "stopwords" -> Some(Seq("de", "het", "een")), "stem_exclusion" -> Some(Seq("fiets")))
      // Fiets should actually be in the stem exclusion list, otherwise the stem will be "fiet".

      val languageAnalyzer3 = LanguageAnalyzer("language3", language = "english", stopwordsPath = Some("/path/to/stopwords"))
      val languageJson3 = Json.obj("name" -> "language3", "type" -> "english", "stopwords_path" -> Some("/path/to/stopwords"))

      "has a JSON representation" in {
        jsonFormatTest(languageAnalyzer1, languageJson1)
        jsonFormatTest(languageAnalyzer2, languageJson2)
        jsonFormatTest(languageAnalyzer3, languageJson3)
      }

      "is recognized when part of a definition object" in {
        analyzerSeqTest(languageAnalyzer1) // english
        analyzerSeqTest(languageAnalyzer2) // dutch
      }

    }

    "have a SnowballAnalyzer class that" >> {

      val snowballAnalyzer1 = SnowballAnalyzer("snowball1")
      val snowballJson1 = Json.obj("name" -> "snowball1")

      val snowballAnalyzer2 = SnowballAnalyzer("snowball2", language = Some("Dutch"), stopwords = Some(Seq("de", "het", "een")))
      val snowballJson2 = Json.obj("name" -> "snowball2", "language" -> "Dutch", "stopwords" -> Some(Seq("de", "het", "een")))

      "has a JSON representation" in {
        jsonFormatTest(snowballAnalyzer1, snowballJson1)
        jsonFormatTest(snowballAnalyzer2, snowballJson2)
      }

      "is recognized when part of a definition object" in {
        analyzerSeqTest(snowballAnalyzer1)
      }

    }

    "have a CustomAnalyzer class that" >> {

      val customAnalyzer1 = CustomAnalyzer("custom1", tokenizer = "standard")
      val customJson1 = Json.obj("name" -> "custom1", "tokenizer" -> "standard")

      val customAnalyzer2 = CustomAnalyzer("custom2", tokenizer = "standard", filter = Some(Seq("filter1", "filter2")), charFilter = Some(Seq("html")))
      val customJson2 = Json.obj("name" -> "custom2", "tokenizer" -> "standard", "filter" -> Some(Seq("filter1", "filter2")), "char_filter" ->Some(Seq("html")))

      "has a JSON representation" in {
        jsonFormatTest(customAnalyzer1, customJson1)
        jsonFormatTest(customAnalyzer2, customJson2)
      }

      "is recognized when part of a definition object" in {
        analyzerSeqTest(customAnalyzer1)
      }

    }

    /* Tokenizer */

    "have a StandardTokenizer class that" >> {

      "has a JSON representation" in {
        jsonFormatTest(StandardTokenizer("standard1"), Json.obj("name" -> "standard1", "max_token_length" -> StandardTokenizer.defaultMaxTokenLength))
        jsonFormatTest(StandardTokenizer("standard2", maxTokenLength = 37), Json.obj("name" -> "standard2", "max_token_length" -> 37))
      }

      "recognized when part of a definition object" in {
        tokenizerSeqTest(StandardTokenizer("standard1"))
      }

    }

    "have a EdgeNGramTokenizer class that" >> {

      "has a JSON representation" in {
        jsonFormatTest(EdgeNGramTokenizer("edgeNGram1"), Json.obj("name" -> "edgeNGram1", "min_gram" -> EdgeNGramTokenizer.defaultMinGram, "max_gram" -> EdgeNGramTokenizer.defaultMaxGram))
        jsonFormatTest( EdgeNGramTokenizer("edgeNGram2", minGram = 2, maxGram = 5, tokenChars = Some(Seq("letter"))),
            Json.obj("name" -> "edgeNGram2", "min_gram" -> 2, "max_gram" -> 5, "token_chars" -> Some(Seq("letter"))) )
      }

      "recognized when part of a definition object" in {
        tokenizerSeqTest(EdgeNGramTokenizer("edgeNGram1"))
      }

    }

    "have a KeywordTokenizer class that" >> {

      "has a JSON representation" in {
        jsonFormatTest(KeywordTokenizer("keyword1"), Json.obj("name" -> "keyword1", "buffer_size" -> KeywordTokenizer.defaultBufferSize))
        jsonFormatTest(KeywordTokenizer("keyword2", bufferSize = 64), Json.obj("name" -> "keyword2", "buffer_size" -> 64))
      }

      "recognized when part of a definition object" in {
        tokenizerSeqTest(KeywordTokenizer("keyword1"))
      }

    }

    "have a LetterTokenizer class that" >> {

      val tokenizer = LetterTokenizer("LetterTokenizer")

      "has a JSON representation" in {
        jsonFormatTest(tokenizer, Json.obj("name" -> "LetterTokenizer"))
      }

      "is recognized when part of a definition object" in {
        tokenizerSeqTest(tokenizer)
      }

    }

    "have a LowercaseTokenizer class that" >> {

      val tokenizer = LowercaseTokenizer("LowercaseTokenizer")

      "has a JSON representation" in {
        jsonFormatTest(tokenizer, Json.obj("name" -> "LowercaseTokenizer"))
      }

      "is recognized when part of a definition object" in {
        tokenizerSeqTest(tokenizer)
      }

    }

    "have a NGramTokenizer class that" >> {

      "has a JSON representation" in {
        jsonFormatTest(NGramTokenizer("nGram1"), Json.obj("name" -> "nGram1", "min_gram" -> NGramTokenizer.defaultMinGram, "max_gram" -> NGramTokenizer.defaultMaxGram))
        jsonFormatTest(NGramTokenizer("nGram2", minGram = 2, maxGram = 5, tokenChars = Some(Seq("letter"))),
            Json.obj("name" -> "nGram2", "min_gram" -> 2, "max_gram" -> 5, "token_chars" -> Some(Seq("letter"))) )
      }

      "recognized when part of a definition object" in {
        tokenizerSeqTest(NGramTokenizer("nGram1"))
      }

    }

    "have a WhitespaceTokenizer class that" >> {

      val tokenizer = WhitespaceTokenizer("WhitespaceTokenizer")

      "has a JSON representation" in {
        jsonFormatTest(tokenizer, Json.obj("name" -> "WhitespaceTokenizer"))
      }

      "is recognized when part of a definition object" in {
        tokenizerSeqTest(tokenizer)
      }

    }

    "have a PatternTokenizer class that" >> {

      "has a JSON representation" in {
        jsonFormatTest(PatternTokenizer("pattern1"), Json.obj("name" -> "pattern1"))
        jsonFormatTest(PatternTokenizer("pattern2", pattern = Some(""",\s*"""), flags = Some("CASE_INSENSITIVE|COMMENTS"), group = Some(0)),
            Json.obj("name" -> "pattern2", "pattern" -> Some(""",\s*"""), "flags" -> Some("CASE_INSENSITIVE|COMMENTS"), "group" -> Some(0)))
      }

      "recognized when part of a definition object" in {
        tokenizerSeqTest(PatternTokenizer("pattern1"))
      }

    }

    "have a UaxUrlEmailTokenizer class that" >> {

      "has a JSON representation" in {
        jsonFormatTest(UaxUrlEmailTokenizer("uaxurlemail1"), Json.obj("name" -> "uaxurlemail1", "max_token_length" -> UaxUrlEmailTokenizer.defaultMaxTokenLength))
        jsonFormatTest(UaxUrlEmailTokenizer("uaxurlemail2", maxTokenLength = 37), Json.obj("name" -> "uaxurlemail2", "max_token_length" -> 37))
      }

      "recognized when part of a definition object" in {
        tokenizerSeqTest(UaxUrlEmailTokenizer("uaxurlemail1"))
      }

    }

    "have a PathHierarchyTokenizer class that" >> {

      "has a JSON representation" in {
        jsonFormatTest(PathHierarchyTokenizer("phTokenizer1"),
            Json.obj("name" -> "phTokenizer1", "delimiter" -> PathHierarchyTokenizer.defaultDelimiter, "buffer_size" -> PathHierarchyTokenizer.defaultBufferSize, "reverse" -> PathHierarchyTokenizer.defaultReverse))
        jsonFormatTest(PathHierarchyTokenizer("phTokenizer2", delimiter = ":", replacement = Some("/"), bufferSize = 64, reverse = true, skip = Some(1)),
            Json.obj("name" -> "phTokenizer2", "delimiter" -> ":", "replacement" -> Some("/"), "buffer_size" -> 64, "reverse" -> true, "skip" -> Some(1)))
      }

      "recognized when part of a definition object" in {
        tokenizerSeqTest(PathHierarchyTokenizer("phTokenizer1"))
      }

    }

    /* TokenFilter */

    "have a StandardTokenFilter class that" >> {

      val filter = StandardTokenFilter("StandardTokenFilter")

      "has a JSON representation" in {
        jsonFormatTest(filter, Json.obj("name" -> "StandardTokenFilter"))
      }

      "is recognized when part of a definition object" in {
        tokenFilterSeqTest(filter)
      }

    }

    "have a AsciiFoldingTokenFilter class that" >> {

      val filter = AsciiFoldingTokenFilter("AsciiFoldingTokenFilter")

      "has a JSON representation" in {
        jsonFormatTest(filter, Json.obj("name" -> "AsciiFoldingTokenFilter"))
      }

      "is recognized when part of a definition object" in {
        tokenFilterSeqTest(filter)
      }

    }

    "have a LengthTokenFilter class that" >> {

      "has a JSON representation" in {
        jsonFormatTest(LengthTokenFilter("LengthFilter1"), Json.obj("name" -> "LengthFilter1"))
        jsonFormatTest(LengthTokenFilter("LengthFilter2", min=Some(3), max=Some(12)), Json.obj("name" -> "LengthFilter2", "min" -> Some(3), "max" -> Some(12)))
      }

      "recognized when part of a definition object" in {
        tokenFilterSeqTest(LengthTokenFilter("LengthFilter"))
      }

    }

    "have a LowercaseTokenFilter class that" >> {

      "has a JSON representation" in {
        jsonFormatTest(LowercaseTokenFilter("LowercaseFilter1"), Json.obj("name" -> "LowercaseFilter1"))
        jsonFormatTest(LowercaseTokenFilter("LowercaseFilter2", language=Some("greek")), Json.obj("name" -> "LowercaseFilter2", "language" -> Some("greek")))
      }

      "recognized when part of a definition object" in {
        tokenFilterSeqTest(LowercaseTokenFilter("LowercaseFilter"))
      }

    }

    "have a NGramTokenFilter class that" >> {

      "has a JSON representation" in {
        jsonFormatTest(NGramTokenFilter("nGram1"),
            Json.obj("name" -> "nGram1", "min_gram" -> NGramTokenFilter.defaultMinGram, "max_gram" -> NGramTokenFilter.defaultMaxGram))
        jsonFormatTest(NGramTokenFilter("nGram2", minGram = 2, maxGram = 5),
            Json.obj("name" -> "nGram2", "min_gram" -> 2, "max_gram" -> 5) )
      }

      "recognized when part of a definition object" in {
        tokenFilterSeqTest(NGramTokenFilter("NGramFilter"))
      }

    }

    "have a EdgeNGramTokenFilter class that" >> {

      "has a JSON representation" in {
        jsonFormatTest(EdgeNGramTokenFilter("EdgeNGramFilter1"),
            Json.obj("name" -> "EdgeNGramFilter1", "min_gram" -> EdgeNGramTokenFilter.defaultMinGram, "max_gram" -> EdgeNGramTokenFilter.defaultMaxGram, "side" -> EdgeNGramTokenFilter.defaultSide))
        jsonFormatTest(EdgeNGramTokenFilter("EdgeNGramFilter2", minGram = 2, maxGram = 5, side = "back"),
            Json.obj("name" -> "EdgeNGramFilter2", "min_gram" -> 2, "max_gram" -> 5, "side" -> "back") )
      }

      "recognized when part of a definition object" in {
        tokenFilterSeqTest(NGramTokenFilter("EdgeNGramFilter"))
      }

    }

    "have a PorterStemTokenFilter class that" >> {

      val filter = PorterStemTokenFilter("PorterStemTokenFilter")

      "has a JSON representation" in {
        jsonFormatTest(filter, Json.obj("name" -> "PorterStemTokenFilter"))
      }

      "is recognized when part of a definition object" in {
        tokenFilterSeqTest(filter)
      }

    }

    "have a ShingleTokenFilter class that" >> {

      "has a JSON representation" in {
        jsonFormatTest(ShingleTokenFilter("ShingleFilter1"),
            Json.obj("name" -> "ShingleFilter1", "max_shingle_size" -> ShingleTokenFilter.defaultMaxShingleSize, "min_shingle_size" -> ShingleTokenFilter.defaultMinShingleSize,
                "output_unigrams" -> ShingleTokenFilter.defaultOutputUnigrams, "output_unigrams_if_no_shingles" -> ShingleTokenFilter.defaultOutputUnigramsIfNoShingles))
        jsonFormatTest(ShingleTokenFilter("ShingleFilter2", maxShingleSize=4, minShingleSize=3, outputUnigrams = false, outputUnigramsIfNoShingles = true, tokenSeparator = Some("_")),
            Json.obj("name" -> "ShingleFilter2", "max_shingle_size" -> 4, "min_shingle_size" -> 3, "output_unigrams" -> false, "output_unigrams_if_no_shingles" -> true, "token_separator" -> Some("_")))
      }

      "recognized when part of a definition object" in {
        tokenFilterSeqTest(ShingleTokenFilter("ShingleFilter"))
      }

    }

    "have a StopTokenFilter class that" >> {

      "has a JSON representation" in {
        jsonFormatTest(StopTokenFilter("StopTokenFilter1"),
            Json.obj("name" -> "StopTokenFilter1", "enable_position_increments" -> StopTokenFilter.defaultEnablePositionIncrements, "ignore_case" -> StopTokenFilter.defaultIgnoreCase, "remove_trailing" -> StopTokenFilter.defaultRemoveTrailing))
        jsonFormatTest(StopTokenFilter("StopTokenFilter2", stopwords=Some(Seq("foo", "bar")), enablePositionIncrements=false),
            Json.obj("name" -> "StopTokenFilter2", "stopwords" -> Some(Seq("foo", "bar")), "enable_position_increments" -> false, "ignore_case" -> false, "remove_trailing" -> true))
        jsonFormatTest(StopTokenFilter("StopTokenFilter3", stopwordsPath=Some("where"), ignoreCase=true, removeTrailing=false),
            Json.obj("name" -> "StopTokenFilter3", "stopwords_path" -> Some("where"), "enable_position_increments" -> true, "ignore_case" -> true, "remove_trailing" -> false))
      }

      "recognized when part of a definition object" in {
        tokenFilterSeqTest(StopTokenFilter("StopTokenFilter"))
      }

    }

    "have a WordDelimiterTokenFilter class that" >> {

      "has a JSON representation" in {
        jsonFormatTest(WordDelimiterTokenFilter("WordDelimiterTokenFilter1"),
            Json.obj("name" -> "WordDelimiterTokenFilter1", "generate_word_parts" -> WordDelimiterTokenFilter.defaultGenerateWordParts, "generate_number_parts" -> WordDelimiterTokenFilter.defaultGenerateNumberParts,
                "catenate_words" -> WordDelimiterTokenFilter.defaultCatenateWords, "catenate_numbers" -> WordDelimiterTokenFilter.defaultCatenateNumbers, "catenate_all" -> WordDelimiterTokenFilter.defaultCatenateAll,
                "split_on_case_change" -> WordDelimiterTokenFilter.defaultSplitOnCaseChange, "preserve_original" -> WordDelimiterTokenFilter.defaultPreserveOriginal, "split_on_numerics" -> WordDelimiterTokenFilter.defaultSplitOnNumerics,
                "stem_english_possessive" -> WordDelimiterTokenFilter.defaultStemEnglishPossessive))
        jsonFormatTest(WordDelimiterTokenFilter("WordDelimiterTokenFilter2", generateWordParts = false, generateNumberParts = false, typeTablePath = Some("where")),
            Json.obj("name" -> "WordDelimiterTokenFilter2", "generate_word_parts" -> false, "generate_number_parts" -> false, "catenate_words" -> false, "catenate_numbers" -> false,
                     "catenate_all" -> false, "split_on_case_change" -> true, "preserve_original" -> false, "split_on_numerics" -> true, "stem_english_possessive" -> true, "type_table_path" -> Some("where")))
      }

      "recognized when part of a definition object" in {
        tokenFilterSeqTest(WordDelimiterTokenFilter("WordDelimiterTokenFilter"))
      }

    }

    "have a StemmerTokenFilter class that" >> {

      "has a JSON representation" in {
        jsonFormatTest(StemmerTokenFilter("StemmerTokenFilter1", language="dutch"),
            Json.obj("name" -> "StemmerTokenFilter1", "language" -> "dutch"))
      }

      "may use 'name' instead of 'language'" in {
        TokenFilter.filterListReads.reads(Json.obj("StemmerTokenFilter2" -> Json.obj("type" -> "stemmer", "name" -> "dutch"))) ===
          JsSuccess(Seq(StemmerTokenFilter("StemmerTokenFilter2", language="dutch")))
      }

      "recognized when part of a definition object" in {
        tokenFilterSeqTest(StemmerTokenFilter("StemmerTokenFilter", language="dutch"))
      }

    }

    "have a StemmerOverrideTokenFilter class that" >> {

      "has a JSON representation" in {
        jsonFormatTest(StemmerOverrideTokenFilter("StemmerOverrideTokenFilter1"),
            Json.obj("name" -> "StemmerOverrideTokenFilter1"))
        jsonFormatTest(StemmerOverrideTokenFilter("StemmerOverrideTokenFilter2", rulesPath = Some("where")),
            Json.obj("name" -> "StemmerOverrideTokenFilter2", "rules_path" -> Some("where")))
      }

      "recognized when part of a definition object" in {
        tokenFilterSeqTest(StemmerOverrideTokenFilter("StemmerOverrideTokenFilter"))
      }

    }

    "have a KeywordMarkerTokenFilter class that" >> {

      "has a JSON representation" in {
        jsonFormatTest(KeywordMarkerTokenFilter("KeywordMarkerTokenFilter1", keywordsPath = Some("where")),
            Json.obj("name" -> "KeywordMarkerTokenFilter1", "keywords_path" -> Some("where"), "ignore_case" -> KeywordMarkerTokenFilter.defaultIgnoreCase))
        jsonFormatTest(KeywordMarkerTokenFilter("KeywordMarkerTokenFilter2", keywords = Some(Seq("keyword")), ignoreCase = true),
            Json.obj("name" -> "KeywordMarkerTokenFilter2", "keywords" -> Some(Seq("keyword")), "ignore_case" -> true))
      }

      "recognized when part of a definition object" in {
        tokenFilterSeqTest(KeywordMarkerTokenFilter("KeywordMarkerTokenFilter"))
      }

    }

    "have a KeywordRepeatTokenFilter class that" >> {

      val filter = KeywordRepeatTokenFilter("KeywordRepeatTokenFilter")

      "has a JSON representation" in {
        jsonFormatTest(filter, Json.obj("name" -> "KeywordRepeatTokenFilter"))
      }

      "is recognized when part of a definition object" in {
        tokenFilterSeqTest(filter)
      }

    }

    "have a KStemTokenFilter class that" >> {

      val filter = KStemTokenFilter("KStemTokenFilter")

      "has a JSON representation" in {
        jsonFormatTest(filter, Json.obj("name" -> "KStemTokenFilter"))
      }

      "is recognized when part of a definition object" in {
        tokenFilterSeqTest(filter)
      }

    }

    "have a SnowballTokenFilter class that" >> {

      val filter = SnowballTokenFilter("SnowballTokenFilter", "Spanish")

      "has a JSON representation" in {
        jsonFormatTest(filter, Json.obj("name" -> "SnowballTokenFilter", "language" -> "Spanish"))
      }

      "is recognized when part of a definition object" in {
        tokenFilterSeqTest(filter)
      }

    }

    "have a PhoneticTokenFilter class that" >> {

      val filter1 = PhoneticTokenFilter("PhoneticTokenFilter1", encoder = "metaphone")
      val filter2 = PhoneticTokenFilter("PhoneticTokenFilter2", encoder = "metaphone", replace = false)

      "has a JSON representation" in {
        jsonFormatTest(filter1, Json.obj("name" -> "PhoneticTokenFilter1", "encoder" -> "metaphone", "replace" -> true))
        jsonFormatTest(filter2, Json.obj("name" -> "PhoneticTokenFilter2", "encoder" -> "metaphone", "replace" -> false))
      }

      "is recognized when part of a definition object" in {
        tokenFilterSeqTest(filter1)
      }

    }

    "have a SynonymTokenFilter class that" >> {

      val filter1 = SynonymTokenFilter("SynonymTokenFilter1", synonymsPath = Some("where"))
      val filter2 = SynonymTokenFilter("SynonymTokenFilter2", format = Some("wordnet"),
          synonyms = Some(Seq("s(100000001,1,'abstain',v,1,0).", "s(100000001,2,'refrain',v,1,0).")), ignoreCase = true, expand = false, tokenizer = Some("whitespace"))

      "has a JSON representation" in {
        jsonFormatTest(filter1, Json.obj("name" -> "SynonymTokenFilter1", "synonyms_path" -> Some("where"), "ignore_case" -> SynonymTokenFilter.defaultIgnoreCase, "expand" -> SynonymTokenFilter.defaultExpand))
        jsonFormatTest(filter2, Json.obj("name" -> "SynonymTokenFilter2", "format" -> Some("wordnet"),
            "synonyms" -> Some(Seq("s(100000001,1,'abstain',v,1,0).", "s(100000001,2,'refrain',v,1,0).")), "ignore_case" -> true, "expand" -> false, "tokenizer" -> Some("whitespace")))
      }

      "is recognized when part of a definition object" in {
        tokenFilterSeqTest(filter1)
      }

    }

    "have a DictionaryDecompounderTokenFilter class that" >> {

      val filter1 = DictionaryDecompounderTokenFilter("DictionaryDecompounderTokenFilter1", wordListPath = Some("where"))
      val filter2 = DictionaryDecompounderTokenFilter("DictionaryDecompounderTokenFilter2", wordList = Some(Seq("one", "two")),
          minWordSize = 0, minSubwordSize = 0, maxSubwordSize = 314, onlyLongestMatch = true)

      "has a JSON representation" in {
        jsonFormatTest(filter1, Json.obj("name" -> "DictionaryDecompounderTokenFilter1", "word_list_path" -> Some("where"),
            "min_word_size" -> DictionaryDecompounderTokenFilter.defaultMinWordSize,
            "min_subword_size" -> DictionaryDecompounderTokenFilter.defaultMinSubwordSize,
            "max_subword_size" -> DictionaryDecompounderTokenFilter.defaultMaxSubwordSize,
            "only_longest_match" -> DictionaryDecompounderTokenFilter.defaultOnlyLongestMatch))
        jsonFormatTest(filter2, Json.obj("name" -> "DictionaryDecompounderTokenFilter2", "word_list" -> Some(Seq("one", "two")),
            "min_word_size" -> 0, "min_subword_size" -> 0, "max_subword_size" -> 314, "only_longest_match" -> true))
      }

      "is recognized when part of a definition object" in {
        tokenFilterSeqTest(filter1)
      }

    }

    "have a HyphenationDecompounderTokenFilter class that" >> {

      val filter1 = HyphenationDecompounderTokenFilter("HyphenationDecompounderTokenFilter1", wordListPath = Some("where"))
      val filter2 = HyphenationDecompounderTokenFilter("HyphenationDecompounderTokenFilter2", wordList = Some(Seq("one", "two")),
          minWordSize = 0, minSubwordSize = 0, maxSubwordSize = 314, onlyLongestMatch = true)

      "has a JSON representation" in {
        jsonFormatTest(filter1, Json.obj("name" -> "HyphenationDecompounderTokenFilter1", "word_list_path" -> Some("where"),
            "min_word_size" -> HyphenationDecompounderTokenFilter.defaultMinWordSize,
            "min_subword_size" -> HyphenationDecompounderTokenFilter.defaultMinSubwordSize,
            "max_subword_size" -> HyphenationDecompounderTokenFilter.defaultMaxSubwordSize,
            "only_longest_match" -> HyphenationDecompounderTokenFilter.defaultOnlyLongestMatch))
        jsonFormatTest(filter2, Json.obj("name" -> "HyphenationDecompounderTokenFilter2", "word_list" -> Some(Seq("one", "two")),
            "min_word_size" -> 0, "min_subword_size" -> 0, "max_subword_size" -> 314, "only_longest_match" -> true))
      }

      "is recognized when part of a definition object" in {
        tokenFilterSeqTest(filter1)
      }

    }

    "have a ReverseTokenFilter class that" >> {

      val filter = ReverseTokenFilter("ReverseTokenFilter")

      "has a JSON representation" in {
        jsonFormatTest(filter, Json.obj("name" -> "ReverseTokenFilter"))
      }

      "is recognized when part of a definition object" in {
        tokenFilterSeqTest(filter)
      }

    }

    "have a ElisionTokenFilter class that" >> {

      val filter = ElisionTokenFilter("ElisionTokenFilter", articles = Seq("l", "m", "t", "qu", "n", "s", "j"))

      "has a JSON representation" in {
        jsonFormatTest(filter, Json.obj("name" -> "ElisionTokenFilter", "articles" -> Seq("l", "m", "t", "qu", "n", "s", "j")))
      }

      "is recognized when part of a definition object" in {
        tokenFilterSeqTest(filter)
      }

    }

    "has a TruncateTokenFilter class that" >> {

      val filter1 = TruncateTokenFilter("TruncateTokenFilter1")
      val filter2 = TruncateTokenFilter("TruncateTokenFilter2", length = 7)

      "has a JSON representation" in {
        jsonFormatTest(filter1, Json.obj("name" -> "TruncateTokenFilter1", "length" -> TruncateTokenFilter.defaultLength))
        jsonFormatTest(filter2, Json.obj("name" -> "TruncateTokenFilter2", "length" -> 7))
      }

      "is recognized when part of a definition object" in {
        tokenFilterSeqTest(filter1)
      }

    }

    "has a UniqueTokenFilter class that" >> {

      val filter1 = UniqueTokenFilter("UniqueTokenFilter1")
      val filter2 = UniqueTokenFilter("UniqueTokenFilter2", onlyOnSamePosition = true)

      "has a JSON representation" in {
        jsonFormatTest(filter1, Json.obj("name" -> "UniqueTokenFilter1", "only_on_same_position" -> UniqueTokenFilter.defaultOnlyOnSamePosition))
        jsonFormatTest(filter2, Json.obj("name" -> "UniqueTokenFilter2", "only_on_same_position" -> true))
      }

      "is recognized when part of a definition object" in {
        tokenFilterSeqTest(filter1)
      }

    }

    "has a PatternCaptureTokenFilter class that" >> {

      val patterns = Seq("(\\p{Ll}+|\\p{Lu}\\p{Ll}+|\\p{Lu}+)", "(\\d+)")
      val filter1 = PatternCaptureTokenFilter("PatternCaptureTokenFilter1", patterns = patterns)
      val filter2 = PatternCaptureTokenFilter("PatternCaptureTokenFilter2", patterns = patterns, preserveOriginal = false)

      "has a JSON representation" in {
        jsonFormatTest(filter1, Json.obj("name" -> "PatternCaptureTokenFilter1", "patterns" -> patterns, "preserve_original" -> PatternCaptureTokenFilter.defaultPreserveOriginal))
        jsonFormatTest(filter2, Json.obj("name" -> "PatternCaptureTokenFilter2", "patterns" -> patterns, "preserve_original" -> false))
      }

      "is recognized when part of a definition object" in {
        tokenFilterSeqTest(filter1)
      }

    }

    "has a PatternReplaceTokenFilter class that" >> {

      val filter = PatternReplaceTokenFilter("PatternReplaceTokenFilter", pattern = "[Ss]olr", replacement = "ElasticSearch")

      "has a JSON representation" in {
        jsonFormatTest(filter, Json.obj("name" -> "PatternReplaceTokenFilter", "pattern" -> "[Ss]olr", "replacement" -> "ElasticSearch"))
      }

      "is recognized when part of a definition object" in {
        tokenFilterSeqTest(filter)
      }

    }

    "has a TrimTokenFilter class that" >> {

      val filter = TrimTokenFilter("TrimTokenFilter")

      "has a JSON representation" in {
        jsonFormatTest(filter, Json.obj("name" -> "TrimTokenFilter"))
      }

      "is recognized when part of a definition object" in {
        tokenFilterSeqTest(filter)
      }

    }

    "has a LimitTokenCountTokenFilter class that" >> {

      val filter1 = LimitTokenCountTokenFilter("LimitTokenCountTokenFilter1")
      val filter2 = LimitTokenCountTokenFilter("LimitTokenCountTokenFilter2", maxTokenCount = 8, consumeAllTokens = true)

      "has a JSON representation" in {
        jsonFormatTest(filter1, Json.obj("name" -> "LimitTokenCountTokenFilter1", "max_token_count" ->LimitTokenCountTokenFilter.defaultMaxTokenCount, "consume_all_tokens" -> LimitTokenCountTokenFilter.defaultConsumeAllTokens))
        jsonFormatTest(filter2, Json.obj("name" -> "LimitTokenCountTokenFilter2", "max_token_count" ->8, "consume_all_tokens" -> true))
      }

      "is recognized when part of a definition object" in {
        tokenFilterSeqTest(filter1)
      }

    }

    "has a HunspellTokenFilter class that" >> {

      val filter1 = HunspellTokenFilter("HunspellTokenFilter1")
      val filter2 = HunspellTokenFilter("HunspellTokenFilter1", ignoreCase = Some(true), strictAffixParsing = Some(false))

      "has a JSON representation" in {
        jsonFormatTest(filter1, Json.obj("name" -> "HunspellTokenFilter1"))
        jsonFormatTest(filter2, Json.obj("name" -> "HunspellTokenFilter1", "ignore_case" -> Some(true), "strict_affix_parsing" -> Some(false)))
      }

      "is recognized when part of a definition object" in {
        tokenFilterSeqTest(filter1)
      }

    }

    "has a CommonGramsTokenFilter class that" >> {

      val filter1 = CommonGramsTokenFilter("CommonGramsTokenFilter1", commonWords = Some(Seq("de", "het", "een")))
      val filter2 = CommonGramsTokenFilter("CommonGramsTokenFilter2", commonWordsPath = Some("where"), ignoreCase = true, queryMode = true)

      "has a JSON representation" in {
        jsonFormatTest(filter1, Json.obj("name" -> "CommonGramsTokenFilter1", "common_words" -> Some(Seq("de", "het", "een")), "ignore_case" -> CommonGramsTokenFilter.defaultIgnoreCase, "query_mode" -> CommonGramsTokenFilter.defaultQueryMode))
        jsonFormatTest(filter2, Json.obj("name" -> "CommonGramsTokenFilter2", "common_words_path" -> Some("where"),  "ignore_case" -> true, "query_mode" -> true))
      }

      "is recognized when part of a definition object" in {
        tokenFilterSeqTest(filter1)
      }

    }

    "has a NormalizationTokenFilter class that" >> {

      val filter1 = NormalizationTokenFilter("NormalizationTokenFilter1", language = "arabic")
      val json1 = Json.obj("name" -> "NormalizationTokenFilter1")

      "has a JSON representation" in {
        Json.toJson(filter1) === json1
        NormalizationTokenFilter.reads("arabic").reads(json1).asOpt === Some(filter1)
      }

      "is recognized when part of a definition object" in {
        tokenFilterSeqTest(filter1)
      }

    }

    "has a KeepWordsTokenFilter class that" >> {

      val filter1 = KeepWordsTokenFilter("KeepWordsTokenFilter1", keepWords = Some(Seq("one", "two", "three")))
      val filter2 = KeepWordsTokenFilter("KeepWordsTokenFilter2", keepWordsPath = Some("where"), keepWordsCase = false)

      "has a JSON representation" in {
        jsonFormatTest(filter1, Json.obj("name" -> "KeepWordsTokenFilter1", "keep_words" -> Some(Seq("one", "two", "three")), "keep_words_case" -> KeepWordsTokenFilter.defaultKeepWordsCase))
        jsonFormatTest(filter2, Json.obj("name" -> "KeepWordsTokenFilter2", "keep_words_path" -> Some("where"), "keep_words_case" -> false))
      }

      "is recognized when part of a definition object" in {
        tokenFilterSeqTest(filter1)
      }

    }

    "has a DelimitedPayloadTokenFilter class that" >> {

      val filter1 = DelimitedPayloadTokenFilter("DelimitedPayloadTokenFilter1")
      val filter2 = DelimitedPayloadTokenFilter("DelimitedPayloadTokenFilter2", delimiter = ";", encoding = "identity")

      "has a JSON representation" in {
        jsonFormatTest(filter1, Json.obj("name" -> "DelimitedPayloadTokenFilter1", "delimiter" -> DelimitedPayloadTokenFilter.defaultDelimiter, "encoding" -> DelimitedPayloadTokenFilter.defaultEncoding))
        jsonFormatTest(filter2, Json.obj("name" -> "DelimitedPayloadTokenFilter2", "delimiter" -> ";", "encoding" -> "identity"))
      }

      "is recognized when part of a definition object" in {
        tokenFilterSeqTest(filter1)
      }

    }

    /* CharFilter */

    "have a MappingCharFilter class that" >> {
      import Analysis.mappingsFormat

      "has a JSON representation" in {
        jsonFormatTest(MappingCharFilter("MappingCharFilter1"),
            Json.obj("name" -> "MappingCharFilter1"))
        jsonFormatTest(MappingCharFilter("MappingCharFilter2", mappings = Some(Seq(("ph" -> "f"), ("qu" -> "kw")))),
            Json.obj("name" -> "MappingCharFilter2", "mappings" -> Seq(("ph" -> "f"), ("qu" -> "kw"))))
        jsonFormatTest(MappingCharFilter("MappingCharFilter3", mappingsPath = Some("where")),
            Json.obj("name" -> "MappingCharFilter3", "mappings_path" -> Some("where")))
      }

      "is recognized when part of a definition object" in {
        charFilterSeqTest(MappingCharFilter("MappingCharFilter"))
      }

    }

    "have a HtmlStripCharFilter class that" >> {

      "has a JSON representation" in {
        jsonFormatTest(HtmlStripCharFilter("HtmlStripCharFilter"), Json.obj("name" -> "HtmlStripCharFilter"))
      }

      "is recognized when part of a definition object" in {
        charFilterSeqTest(HtmlStripCharFilter("HtmlStripCharFilter"))
      }

    }

    "have a PatternReplaceCharFilter class that" >> {

      "has a JSON representation" in {
        jsonFormatTest(PatternReplaceCharFilter("PatternReplaceCharFilter1", pattern="sample(.*)", replacement="replacedSample $1"),
            Json.obj("name" -> "PatternReplaceCharFilter1", "pattern" -> "sample(.*)", "replacement" -> "replacedSample $1"))
      }

      "is recognized when part of a definition object" in {
        charFilterSeqTest(PatternReplaceCharFilter("PatternReplaceCharFilter", pattern="sample(.*)", replacement="replacedSample $1"))
      }

    }

  } // "Analysis should"

}
