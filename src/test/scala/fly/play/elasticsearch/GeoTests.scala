package fly.play.elasticsearch

import org.specs2.execute.{ AsResult, Result }
import org.specs2.mutable.{ Around, Specification }
import org.specs2.specification.Scope
import org.specs2.time.NoTimeConversions
import play.api.libs.functional.syntax.{ functionalCanBuildApplicative, toFunctionalBuilderOps, unlift }
import play.api.libs.json.{ Format, JsPath }
import fly.play.elasticsearch.geo.{ GeoBoundingBoxFilter, GeoDistanceFilter, GeoHash, GeoLatLon, GeoPoint, GeoPointMapping }
import fly.play.elasticsearch.geo.DistanceUnit.{ centimeters, kilometers }
import fly.play.elasticsearch.mapping.{ ObjectMapping, StringMapping }
import fly.play.elasticsearch.query.{ FilteredQuery, MatchAllQuery }
import fly.play.elasticsearch.query.Query.queryToElasticSearchQuery

object GeoTests extends Specification with NoTimeConversions with ClientUtils {

  abstract class WithTestIndexWithMapping extends Scope with Around {
    implicit lazy val temporaryIndex = newTemporaryIndex

    def around[T: AsResult](t: => T): Result = {
      createTestIndexWithMapping
      try {
        AsResult.effectively(t)
      } finally {
        deleteTestIndex
      }
    }
  }

  "GeoPoint should" >> {

    "have a GeoPointLatLon subclass" >> {

      "with Reads and Writes" in {
        val rhinofly: GeoPoint = GeoLatLon(lat = 52.07027, lon = 5.07924)
        rhinofly.toJson.as[GeoPoint] === rhinofly
      }

    }

    "have a GeoHash subclass" >> {

      "with Reads and Writes" in {
        val rhinofly: GeoPoint = GeoHash("u1785x4e6cg")
        rhinofly.toJson.as[GeoPoint] === rhinofly
      }

    }

    "be indexable in ElasticSearch, so that places can be" >> {

      "retrieved" in new WithTestIndexWithMapping {

        places.map { index(_) }
        refreshTestIndex
        val result = search[Place](MatchAllQuery())
        result.hitsTotal === places.length

      }

      "found if within a GeoBoundingBoxFilter" in new WithTestIndexWithMapping {
        places.map { index(_) }
        refreshTestIndex
        val result = search[Place](FilteredQuery(
          // Filter between New York and Helsinki.
          filter = GeoBoundingBoxFilter("position", topLeft = GeoLatLon(60.17332, -73.938611), bottomRight = GeoLatLon(40.664167, 24.94102))))
        result.hitsTotal === 4
        result.hits.map(_.source.name).toSet === Set("Rhinofly", "Amsterdam", "New York", "Helsingfors")
      }

      "found if within a GeoDistanceFilter using kilometers" in new WithTestIndexWithMapping {
        places.map { index(_) }
        refreshTestIndex
        val result = search[Place](FilteredQuery(
          filter = GeoDistanceFilter("position", rhinofly.position, 50, kilometers)))
        result.hitsTotal === 2
        result.hits.map(_.source.name).toSet === Set("Rhinofly", "Amsterdam")
      }

      "found if within a GeoDistanceFilter using centimeters" in new WithTestIndexWithMapping {
        places.map { index(_) }
        refreshTestIndex
        val result = search[Place](FilteredQuery(
          filter = GeoDistanceFilter("position", rhinofly.position, 50, centimeters)))
        result.hitsTotal === 1
        result.hits.map(_.source.name).toSet === Set("Rhinofly")
      }

    }

  }

  case class Place(name: String, position: GeoPoint)
  implicit val placeFormat: Format[Place] = (
    (JsPath \ "name").format[String] and
    (JsPath \ "position").format[GeoPoint])(Place.apply, unlift(Place.unapply))

  val rhinofly = Place("Rhinofly", GeoHash("u1785x4e6cg")) // Really, check http://geohash.org/u1785x4e6cg
  val places = Seq(
    rhinofly,
    Place("Amsterdam", GeoLatLon(52.37022, 4.89517)),
    Place("New York", GeoLatLon(lat = 40.664167, lon = -73.938611)),
    Place("Helsingfors", GeoLatLon("60.17332, 24.94102")),
    Place("Lisboa", GeoHash("eycs0p8ukc7v")),
    Place("Sydney", GeoLatLon(-33.859972, 151.211111)))

  def createTestIndexWithMapping(implicit i: TemporaryIndex) =
    awaitResult(testIndex.create(Seq(
      ObjectMapping(i.typeName, properties = Set(
        StringMapping("name"),
        GeoPointMapping("position", indexLatLon = true))))))

}
