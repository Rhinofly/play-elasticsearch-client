package fly.play.elasticsearch.mapping

import org.specs2.execute.{AsResult, Result}
import org.specs2.matcher.TraversableMatchers
import org.specs2.mutable.{Around, Specification}
import org.specs2.specification.Scope
import org.specs2.time.NoTimeConversions
import play.api.libs.json.{JsObject, Json}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import fly.play.elasticsearch.{ClientUtils, ElasticSearchException}
import fly.play.elasticsearch.geo.GeoPointMapping
import fly.play.elasticsearch.query.MultiMatchQuery
import fly.play.elasticsearch.query.Query.queryToElasticSearchQuery

object MappingTests extends Specification with NoTimeConversions with ClientUtils {

  sequential

    "Mapping should" >> {

      /**
       * Perform tests on a mapping and its corresponding JSON representation.
       */
      def mappingTest(mapping: Mapping, json: JsObject) = {
        "JSON representation (toJson)" ! (mapping.toJson === json)
        "Round-trip from JSON to JSON (fromJson)" ! (Mapping.fromJsonRoot(json.fields.head).toJson === json)
      }

      "have a StringMapping sub-class" in {
        val mapping = StringMapping("name", termVector=TermVectorType.with_offsets, analyzer="english",
          store=StoreType.yes, index=IndexType.default, boost=2.0, nullValue=Option(""), includeInAll=Option(true)
        )
        val json = Json.obj("name" -> Json.obj("type" -> "string", "term_vector" -> "with_offsets", "analyzer" -> "english",
          "store" -> true, "boost" -> 2.0, "null_value" -> "", "include_in_all" -> true
        ))
        mappingTest(mapping, json)
      }

      "have a ObjectMapping sub-class" in {
        val mapping1 = ObjectMapping("person", properties = Set(
          StringMapping("first_name"), StringMapping("last_name")
        ))
        val json1 = Json.obj("person" -> Json.obj("type" -> "object", "properties" -> Json.obj(
            "first_name" -> Json.obj("type" -> "string"), "last_name" -> Json.obj("type" -> "string")
        )))
        val mapping2 = ObjectMapping("object2", dynamic = false, properties = Set(StringMapping("string")))
        val json2 = Json.obj("object2" -> Json.obj("type" -> "object", "dynamic" -> false, "properties" -> Json.obj("string" -> Json.obj("type" -> "string"))))
        mappingTest(mapping1, json1)
        mappingTest(mapping2, json2)
      }

      "have a NumberMapping sub-class" in {
        val mapping = NumberMapping("int", NumberType.integer, precisionStep=2, ignoreMalformed=true,
          boost=2.0, nullValue=Option(""), includeInAll=Option(true)
        )
        val json = Json.obj("int" -> Json.obj("type" -> "integer", "precision_step" -> 2, "ignore_malformed" -> true,
          "boost" -> 2.0, "null_value" -> "", "include_in_all" -> true
        ))
        mappingTest(mapping, json)
      }

      "have a TokenCountMapping sub-class" in {
        val mapping = TokenCountMapping("tokens", precisionStep=1, ignoreMalformed=NumberMapping.defaultIgnoreMalformed,
          boost=2.0, analyzer="weird"
        )
        val json = Json.obj("tokens" -> Json.obj("type" -> "token_count", "precision_step" -> 1,
          "boost" -> 2.0, "analyzer" -> "weird"
        ))
        mappingTest(mapping, json)
      }

      "have a DateMapping sub-class" in {
        val mapping = DateMapping("when", precisionStep=1, ignoreMalformed=true,
          format="YYYY-MM-dd"
        )
        val json = Json.obj("when" -> Json.obj("type" -> "date", "precision_step" -> 1, "ignore_malformed" -> true,
          "format" -> "YYYY-MM-dd"
        ))
        mappingTest(mapping, json)
      }

      "have a BooleanMapping sub-class" in {
        val mapping = BooleanMapping("ok", boost=10.0)
        val json = Json.obj("ok" -> Json.obj("type" -> "boolean", "boost" -> 10.0))
        mappingTest(mapping, json)
      }

      "have a BinaryMapping sub-class" in {
        val mapping = BinaryMapping("image")
        val json = Json.obj("image" -> Json.obj("type" -> "binary"))
        mappingTest(mapping, json)
      }

      "have a GeoPointMapping sub-class" in {
        val mapping = GeoPointMapping("position", indexLatLon = true)
        val json = Json.obj("position" -> Json.obj("type" -> "geo_point", "lat_lon" -> true))
        mappingTest(mapping, json)
      }

      "have a withFieldData method" >> {
        val fieldData = FieldData(filter = FieldDataFilter(regexPattern = "^#.*"))

        "which yields a MappingWithFieldData that behaves like a normal Mapping" in {
          val mapping = StringMapping("text").withFieldData(fieldData)
          val json = Json.obj("text" -> Json.obj(
            "type" -> "string", "fielddata" -> Json.obj(
              "filter" -> Json.obj("regex" -> Json.obj("pattern" -> "^#.*"))
            )
          ))
          mappingTest(mapping, json)
        }

        "which cannot be called more than once" in {
          val mapping = StringMapping("text").withFieldData(fieldData)
          mapping.withFieldData(fieldData) must throwA[ElasticSearchException]
        }

      }

      "have a RootObjectMapping sub-class" >> {
        val rootProperties1 = RootProperties(indexAnalyzer = Some("standard"), searchAnalyzer = Some("special"))
        val rootProperties2 = RootProperties(analyzer = Some("standard"), dynamicDateFormats = Some(Seq("yyyy-MM-dd", "dd-MM-yyyy")), dateDetection = false, numericDetection = true)

        "which behaves like a normal mapping" in {

          val objectMapping = ObjectMapping("object", properties = Set(StringMapping("string")))
          val mapping1 = RootObjectMapping(objectMapping, rootProperties = rootProperties1)
          val json1 = Json.obj("object" -> Json.obj("type" -> "object",
              "properties" -> Json.obj("string" -> Json.obj("type" -> "string")),
              "index_analyzer" -> "standard", "search_analyzer" -> "special"
            ))
          val mapping2 = RootObjectMapping(objectMapping, rootProperties = rootProperties2)
          val json2 = Json.obj("object" -> Json.obj("type" -> "object",
              "properties" -> Json.obj("string" -> Json.obj("type" -> "string")),
              "analyzer" -> "standard", "dynamic_date_formats" -> Some(Seq("yyyy-MM-dd", "dd-MM-yyyy")), "date_detection" -> false, "numeric_detection" -> true
            ))
          mappingTest(mapping1, json1)
          mappingTest(mapping2, json2)

        }

        "which can be combined with fielddata" in {
          val fieldData = FieldData(filter = FieldDataFilter(regexPattern = "^#.*"))
          val objectMapping = ObjectMapping("object", properties = Set(StringMapping("string")))
          val mapping1 = RootObjectMapping(objectMapping, rootProperties = rootProperties1).withFieldData(fieldData)
          val mapping2 = RootObjectMapping(objectMapping.withFieldData(fieldData), rootProperties = rootProperties1)
          val json = Json.obj("object" -> Json.obj("type" -> "object",
              "properties" -> Json.obj("string" -> Json.obj("type" -> "string")),
              "fielddata" -> Json.obj( "filter" -> Json.obj("regex" -> Json.obj("pattern" -> "^#.*")) ),
              "index_analyzer" -> "standard", "search_analyzer" -> "special"
            ))
          mappingTest(mapping1, json)
          mappingTest(mapping2, json)
        }

      }

      "have a MultiFieldMapping sub-class" in {
        val mapping = MultiFieldMapping("multi", fields = Seq(StringMapping("analyzed"), StringMapping("not-analyzed", index = IndexType.not_analyzed)))
        val json = Json.obj("multi" -> Json.obj("type" -> "multi_field", "fields" -> Json.obj(
            "analyzed" -> Json.obj("type" -> "string"), "not-analyzed" -> Json.obj("type" -> "string", "index" -> "not_analyzed")
        )))
        mappingTest(mapping, json)
      }

      "have a NestedMapping sub-class" in {
        val mapping = NestedMapping("computer", properties = Set(
            StringMapping("make"), StringMapping("model")
          ))
        val json = Json.obj("computer" -> Json.obj("type" -> "nested", "properties" -> Json.obj(
            "make" -> Json.obj("type" -> "string"), "model" -> Json.obj("type" -> "string")
        )))
        mappingTest(mapping, json)
      }

    } // "Mapping should"

    "index should" >> {

      "have a create method that defines mappings for the index" in {
        if (existsTestIndex) deleteTestIndex
        val result = createTestIndexWithMapping
        result === ()
      }

      "have a mappings method to get the mappings" in new WithTestIndexWithMapping {
        val mappingResults: Seq[Mapping] = awaitResult(testIndex.mappings)
        mappingResults.length === 1
        equalMappings(mappingResults(0), testMapping)
      }

      "type should" >> {

        "have a create method to define a mapping" in new WithTestIndex {
          awaitResult(testType.create(testMapping))
          val mappingResult = awaitResult(testType.mapping)
          equalMappings(mappingResult, testMapping)
        }

        "throw an exception when defining an invalid mapping" in new WithTestIndex {
          val badMapping = ObjectMapping(testTypeName, properties = Set(
              StringMapping("textField", store = StoreType.yes, index = IndexType.analyzed, analyzer = "total nonsense")
            ))
          awaitResult(testType.create(badMapping)) must throwA[ElasticSearchException]
        }

        "have a mapping method to get a mapping" in new WithTestIndexWithMapping {
          val mappingResult = awaitResult(testType.mapping)
          equalMappings(mappingResult, testMapping)
        }

        "have a mapping method that throws an exception for a non-existent mapping" in new WithTestIndex {
          awaitResult(testType.mapping) must throwA[ElasticSearchException]
        }

        "have a mappingOpt method to get a mapping" in new WithTestIndexWithMapping {
          val mappingResult = awaitResult(testType.mappingOpt).get
          equalMappings(mappingResult, testMapping)
        }

        "have a mappingOpt method that returns None for a non-existent mapping" in new WithTestIndex {
          awaitResult(testType.mappingOpt) === None
        }

        "use the mapping for indexing" in new WithTestIndexWithMapping {
          val (id, version) = index(doc = testDocumentJson, "refresh" -> "true")
          val optionalTestDocument = getV[JsObject](id = id)
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

      } // "type should"

    } // "index should"


  val testMapping =
    ObjectMapping(testTypeName, properties = Set(
      StringMapping("stringField", store = StoreType.yes, index = IndexType.not_analyzed),
      StringMapping("textField", store = StoreType.yes, index = IndexType.analyzed, analyzer = "simple", boost = 3.0),
      NumberMapping("integerField", NumberType.integer),
      ObjectMapping("objectField", properties =  Set(
        DateMapping("dateField"),
        BooleanMapping("booleanField", index=IndexType.no)
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

  def equalMappings[M <: Mapping](a: M, b: M) =
    a match {
      case ao: ObjectMapping => b match {
        case bo: ObjectMapping => {
          ao.field === bo.field ;
          ao.dynamic === bo.dynamic ;
          ao.enabled === bo.enabled ;
          ao.path === bo.path ;
          ao.properties  === bo.properties
        }
        case _ => a === b
      }
      case _ => a === b
    }

  def createTestIndexWithMapping = awaitResult(testIndex.create(Seq(testMapping)))

  abstract class WithTestIndexWithMapping extends Scope with Around {
    def around[T: AsResult](t: => T): Result = {
      if (existsTestIndex) deleteTestIndex
      createTestIndexWithMapping
      AsResult(t)
    }
  }

}