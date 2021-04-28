package dev.nigredo

import dev.nigredo.FallbackValueValidationModelSpec.Example
import eu.timepit.refined.collection.NonEmpty
import org.specs2.mutable.Specification

class FallbackValueValidationModelSpec extends Specification {
  "FallbackValueValidationModel" should {
    "return valid value" in (Example("value").value must beEqualTo("value"))
    "return invalid value" in (Example("").value must beEqualTo("invalid"))
  }
}

object FallbackValueValidationModelSpec {
  object Example extends FallbackValueValidationModel[String, NonEmpty, "invalid"]
}