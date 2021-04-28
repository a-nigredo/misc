package dev.nigredo

import eu.timepit.refined.api.{Refined, Validate}
import eu.timepit.refined.boolean.Or
import eu.timepit.refined.generic.Equal
import eu.timepit.refined.refineV
import shapeless.tag
import shapeless.tag.@@

/**
 * Model checks if value R can pass predicate P otherwise return fallback value FV
 */
trait FallbackValueValidationModel[R, P, FV <: R] {

  final type Predicate = Or[P, Equal[FV]]
  final type Raw = R
  final type FallbackValue = FV
  final type T = Result @@ this.type

  private type Result = R Refined Predicate

  final def apply(value: R)(implicit ev: Validate[Raw, Predicate], wt: ValueOf[FV]): T =
    tag[this.type][Result](refineV(value).getOrElse(Refined.unsafeApply(wt.value)))
}
