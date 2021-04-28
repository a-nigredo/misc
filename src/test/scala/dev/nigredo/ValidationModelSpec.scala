package dev.nigredo

import dev.nigredo.ValidationModelSpec.Example
import eu.timepit.refined.collection.NonEmpty
import org.specs2.mutable.Specification
import org.specs2.matcher.ValidatedMatchers._

class ValidationModelSpec extends Specification {
  "Validation model" should {
    "return valid value" in (Example("foo").map(_.value) must beValid("foo"))
    "return error" in (Example("") must beInvalid)
    "return custom error message" in {

      implicit val inst0: ValidationModel.ErrorMsg[Example.T, Example.Raw, String] =
        ValidationModel.ErrorMsg.instance(_ => "boom!")

      Example.withErr[String]("") must beInvalid("boom!")
    }
  }
}

object ValidationModelSpec {
  object Example extends ValidationModel[String, NonEmpty]
}
