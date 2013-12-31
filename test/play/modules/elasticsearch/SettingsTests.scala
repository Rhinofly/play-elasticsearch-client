package play.modules.elasticsearch

import org.specs2.execute.AsResult
import org.specs2.execute.Result
import org.specs2.runner.ConsoleLogger
import org.specs2.mutable.Around
import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import org.specs2.time.NoTimeConversions
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.modules.elasticsearch.query.MultiMatchQuery
import play.modules.elasticsearch.query.TermQuery
import play.modules.elasticsearch.analysis.{Analysis, StandardAnalyzer}

object SettingsTests extends Specification with NoTimeConversions with ClientUtils {

  sequential

    "index should" >> {

      "have a create method that defines settings and mappings for the index" in {
        if (existsTestIndex) deleteTestIndex
        val result = createTestIndexWithSettingsAndMapping
        deleteTestIndex
        result === ()
      }

//      "have a settings method to get the settings" in new WithTestIndexSettingsAndMapping {
//        val settingsResults: Settings = awaitResult(testIndex.settings)
//        equalSettings(settingsResults, testSettings) === true
//      }

    }

  val testMapping =
    Mapping(testTypeName, properties = Seq(
      Mapping("stringField", fieldType = MappingType.string, store = StoreType.yes, index = IndexType.not_analyzed),
      Mapping("textField", fieldType = MappingType.string, store = StoreType.yes, index = IndexType.analyzed, analyzer = "simple", boost = 3.0),
      Mapping("integerField", fieldType = MappingType.integer),
      Mapping("objectField", properties =  Seq(
        Mapping("dateField", fieldType = MappingType.date),
        Mapping("booleanField", fieldType = MappingType.boolean, index=IndexType.no)
      ))
    ))

  val testSettings =
    Settings(
      nrOfShards = 2,
      nrOfReplicas = 3,
      analysis = Some(Analysis(
        analyzers = Seq(
          StandardAnalyzer(name="standard", stopwords = Some(Seq("de", "het", "een")))
        ),
        tokenizers = Seq.empty,
        filters = Seq.empty
      ))
    )

  def equalSettings(a: Settings, b: Settings): Boolean = {
    def test[T](a: T, b: T, reason: String): Boolean =
      if (a == b) true else {
        ConsoleLogger.error(s"$reason : $a is not equal to $b")
        false
      }
    test(a.nrOfShards, b.nrOfShards, "nrOfShards ") &&
    test(a.nrOfReplicas, b.nrOfReplicas, "nrOfReplicas") &&
    a.analysis.map{aa => b.analysis match {
      case Some(ba) =>
        aa.analyzers.zip(ba.analyzers).forall{ case (anlzr, bnlzr) =>
          test(anlzr, bnlzr, "analyzers")
        }
      case None => test(a.analysis, b.analysis, "Missing Analysis")
    }}.getOrElse(test(a.analysis, b.analysis, "Missing Analysis"))
  }

  def createTestIndexWithSettingsAndMapping = awaitResult(testIndex.create(testSettings, Seq(testMapping)))

  abstract class WithTestIndexSettingsAndMapping extends Scope with Around {
    def around[T: AsResult](t: => T): Result = {
      if (existsTestIndex) deleteTestIndex
        createTestIndexWithSettingsAndMapping
      try {
        AsResult(t)
      } finally {
        deleteTestIndex
      }
    }
  }
}
