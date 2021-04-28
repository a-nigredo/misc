package dev.nigredo

import cats.data.Validated
import eu.timepit.refined.api.{Refined, Validate}
import eu.timepit.refined.refineV
import shapeless.tag
import shapeless.tag.@@
import cats.syntax.either._
import scala.annotation.implicitNotFound

/**
 * Model checks if value R can pass predicate P
 */
trait ValidationModel[R, P] {

  final type Predicate = P
  final type Raw = R
  final type T = Raw Refined Predicate
  final type ErrMsg[A] = ValidationModel.ErrorMsg[T, Raw, A]

  //Validate R and set custom error E in case of error
  final def withErr[E: ErrMsg](value: R)(implicit vl: Validate[Raw, Predicate]): Validated[E, T] @@ this.type =
    tag[this.type][Validated[E, T]](
      refineV(value).toValidated.leftMap(_ => ValidationModel.ErrorMsg[T, R, E].get(value))
    )

  final def apply(value: R)(implicit vl: Validate[Raw, Predicate]): Validated[String, T] @@ this.type =
    tag[this.type][Validated[String, T]](refineV(value).toValidated)
}

object ValidationModel {

  @implicitNotFound("No instance of ErrorMsg[${A}, ${B}, ${C}] in scope")
  trait ErrorMsg[A, B, C] {
    def get(value: B): C
  }

  object ErrorMsg {

    def apply[A, B, C: ErrorMsg[A, B, *]]: ErrorMsg[A, B, C] = implicitly

    def instance[A, B, C](f: B => C): ErrorMsg[A, B, C] = f(_)
  }
}