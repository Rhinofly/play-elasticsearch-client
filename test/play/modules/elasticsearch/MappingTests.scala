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

object MappingTests extends Specification with NoTimeConversions with ClientUtils {

  sequential

    "index should" >> {

      "have a create method that defines mappings for the index" in {
        if (existsTestIndex) deleteTestIndex
        val result = createTestIndexWithMapping
        deleteTestIndex
        result === ()
      }

      "have a mappings method to get the mappings" in new WithTestIndexWithMapping {
        val mappingResults: Seq[Mapping] = awaitResult(testIndex.mappings)
        mappingResults.length === 1
        equalMappings(mappingResults(0), testMapping) === true
      }

      "type should" >> {

        "have a create method to define a mapping" in new WithTestIndex {
          awaitResult(testType.create(testMapping))
          val mappingResult = awaitResult(testType.mapping)
          equalMappings(mappingResult, testMapping) === true
        }

        "throw an exception when defining an invalid mapping" in new WithTestIndex {
          val badMapping = Mapping(testTypeName, properties = Seq(Mapping("textField", fieldType = MappingType.string, store = StoreType.yes, index = IndexType.analyzed, analyzer = "total nonsense")))
          awaitResult(testType.create(badMapping)) must throwA[ElasticSearchException]
        }

        "have a mapping method to get a mapping" in new WithTestIndexWithMapping {
          val mappingResult = awaitResult(testType.mapping)
          equalMappings(mappingResult, testMapping) === true
        }

        "have a mapping method that throws an exception for a non-existent mapping" in new WithTestIndex {
          awaitResult(testType.mapping) must throwA[ElasticSearchException]
        }

        "have a mappingOpt method to get a mapping" in new WithTestIndexWithMapping {
          val mappingResult = awaitResult(testType.mappingOpt).get
          equalMappings(mappingResult, testMapping) === true
        }

        "have a mappingOpt method that returns None for a non-existent mapping" in new WithTestIndex {
          awaitResult(testType.mappingOpt) === None
        }

        "use the mapping for indexing" in new WithTestIndexWithMapping {
          val (id, version) = index(doc = testDocumentJson, "refresh" -> "true")
          val optionalTestDocument = get[JsObject](id = id)
          optionalTestDocument must beLike {
            case Some((v, doc)) =>
              v === version
              doc === testDocumentJson
          }
        }

        "throw an exception when indexing a document which does not conform to the mapping" in new WithTestIndexWithMapping {
          index(doc = testDocumentJson ++ Json.obj("integerField" -> "bad"), "refresh" -> "true") must throwA[ElasticSearchException]
        }

        "use the mapping when searching" in new WithTestIndexWithMapping {
          index(id = "1", doc = testDocumentJson ++ Json.obj("stringField" -> "find me please"))
          index(id = "2", doc = testDocumentJson ++ Json.obj("stringField" -> "find me", "textField" -> "find me"))
          refreshTestIndex
          val result1 = search[JsObject](MultiMatchQuery(fields = Seq("stringField"), value = "find me please"))
          result1.hits.length === 1 // not_analyzed only finds the perfect match
          val result2 = search[JsObject](MultiMatchQuery(fields = Seq("stringField", "textField"), value = "find me please"))
          result2.hits.map(_.id) === List("2", "1")
        }

      }

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

  val testDocumentJson: JsObject = Json.obj(
    "stringField" -> "",
    "textField" -> "",
    "integerField" -> 0,
    "objectField" -> Json.obj(
      "dateField" -> new java.util.Date(),
      "booleanField" -> false
    )
  )

  def equalMappings(a: Mapping, b: Mapping): Boolean = {
    def test[T](a: T, b: T, reason: String): Boolean =
      if (a == b) true else {
        ConsoleLogger.error(s"$reason : $a is not equal to $b")
        false
      }
    test(a.field, b.field, "field ") &&
    test(a.fieldType, b.fieldType, "fieldType") &&
    test(a.index, b.index, "index") &&
    test(a.store, b.store, "store") &&
    test(a.boost, b.boost, "boost") &&
    test(a.properties.map(_.field).toSet[String], b.properties.map(_.field).toSet[String], "properties fields") &&
    a.properties.forall(am => test(equalMappings(am, b.properties.find(bm => bm.field == am.field).get), true, s"property ${am.field}"))
  }

  def createTestIndexWithMapping = awaitResult(testIndex.create(Seq(testMapping)))

  abstract class WithTestIndexWithMapping extends Scope with Around {
    def around[T: AsResult](t: => T): Result = {
      if (existsTestIndex) deleteTestIndex
      createTestIndexWithMapping
      try {
        AsResult(t)
      } finally {
        deleteTestIndex
      }
    }
  }

}