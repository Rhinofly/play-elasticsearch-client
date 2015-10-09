package fly.play.elasticsearch

import org.specs2.mutable.Specification
import fly.play.elasticsearch.filter.RangeExecution
import fly.play.elasticsearch.geo.DistanceType
import fly.play.elasticsearch.geo.OptimizeBbox
import fly.play.elasticsearch.mapping.IndexType
import fly.play.elasticsearch.mapping.TermVectorType
import fly.play.elasticsearch.mapping.MultiFieldMapping
import fly.play.elasticsearch.query.SortOrder
import fly.play.elasticsearch.query.SortMode

object EnumerationDefaultTests extends Specification {

  type Default = { def default: Any }
  type EnumWithDefault = Enumeration with Default

  // Testing is quite tricky because the results change a bit random
  // depending on which field is declared first in the resulting
  // bytecode
  def test[T <: EnumWithDefault](enum: T)(
    getDefault: T => T#Value, name: String) =

    s"$name of ${enum.getClass.getSimpleName}" in {
      import scala.language.reflectiveCalls

      val isDefaultField = enum.default === getDefault(enum)
      val nameIsCorrect = getDefault(enum).toString === name
      val isNotDeclaredAsVal =
        enum.getClass.getDeclaredField("default") must throwA[NoSuchFieldException]

      isNotDeclaredAsVal and isDefaultField and nameIsCorrect
    }

  "Default values of enumerations" should {
    "not be named default" >> {
      test(RangeExecution)(_.index, "index")
      test(DistanceType)(_.arc, "arc")
      test(OptimizeBbox)(_.memory, "memory")
      test(IndexType)(_.analyzed, "analyzed")
      test(TermVectorType)(_.no, "no")
      test(MultiFieldMapping.PathType)(_.full, "full")
      test(SortOrder)(_.desc, "desc")
      test(SortMode)(_.none, "none")
    }
  }
}
