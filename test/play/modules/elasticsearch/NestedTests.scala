package play.modules.elasticsearch

import org.specs2.execute.{AsResult, Result}
import org.specs2.matcher.TraversableMatchers
import org.specs2.mutable.{Around, Specification}
import org.specs2.specification.Scope
import org.specs2.time.NoTimeConversions
import play.api.libs.json.{JsArray, JsObject, Json}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.modules.elasticsearch.filter._
import play.modules.elasticsearch.mapping._
import play.modules.elasticsearch.query._
import play.modules.elasticsearch.query.Query.queryToElasticSearchQuery

class NestedTests extends Specification with NoTimeConversions with ClientUtils {

  def hitNames(result: SearchResult[JsObject]) = result.hits.map(p => (p.source \ "name").as[String])
  val query = BoolQuery(musts = Seq(MatchQuery("computer.make", "Apple"), MatchQuery("computer.model", "UX31a")))

  sequential

  "Nested objects" should {

    "not be found as multivalued fields" in new WithTestIndexWithMapping {
      val result = search[JsObject](query)
      result.hitsTotal === 0
    }

    "be found as multivalued fields when includeInRoot" in new WithTestIndexWithMappingIncludeInRoot {
      val result = search[JsObject](query)
      hitNames(result) must containTheSameElementsAs(Seq("Nobody", "Rich"))
    }

    "be found when they match a NestedQuery" in new WithTestIndexWithMapping {
      val result = search[JsObject](NestedQuery(path = "computer", query = query))
      hitNames(result) must containTheSameElementsAs(Seq("Nobody"))
    }

    "only be found when satisfying a NestedFilter" in new WithTestIndexWithMapping {
      val result = search[JsObject](FilteredQuery(filter = NestedFilter(path = "computer", filter = TermFilter("computer.make", "Apple"))))
      hitNames(result) must containTheSameElementsAs(Seq("Paco", "Nobody", "Rich"))
    }

  }

  val mac = Json.obj("make" -> "Apple", "model" -> "Macbook Pro")
  val pc = Json.obj("make" -> "Asus", "model" -> "UX31a")
  val p1 = Json.obj("name" -> "Paco", "computer" -> mac)
  val p2 = Json.obj("name" -> "Nico", "computer" -> pc)
  val p3 = Json.obj("name" -> "Rich", "computer" -> JsArray(Seq(mac, pc)))
  val p4 = Json.obj("name" -> "Nobody", "computer" -> Json.obj("make" -> "Apple", "model" -> "UX31a"))
  val people = Seq(p1, p2, p3, p4)

  abstract class WithTestIndexWithMapping extends Scope with Around {
    def around[T: AsResult](t: => T): Result = {
      val mapping =
        ObjectMapping(testTypeName, properties = Set(
          StringMapping("name", store = StoreType.yes, index = IndexType.not_analyzed),
          NestedMapping("computer", properties = Set(
            StringMapping("make", index = IndexType.not_analyzed), StringMapping("model", index = IndexType.not_analyzed)
          ))
        ))
      if (existsTestIndex) deleteTestIndex
      awaitResult(testIndex.create(Seq(mapping)))
      people map {p => index(doc = p)}
      refreshTestIndex
      AsResult(t)
    }
  }

  abstract class WithTestIndexWithMappingIncludeInRoot extends Scope with Around {
    def around[T: AsResult](t: => T): Result = {
      val mapping =
        ObjectMapping(testTypeName, properties = Set(
          StringMapping("name", store = StoreType.yes, index = IndexType.not_analyzed),
          NestedMapping("computer", includeInRoot = true, properties = Set(
            StringMapping("make"), StringMapping("model")
          ))
        ))
      if (existsTestIndex) deleteTestIndex
      awaitResult(testIndex.create(Seq(mapping)))
      people map {p => index(doc = p)}
      refreshTestIndex
      AsResult(t)
    }
  }

}