package dev.nigredo

import shapeless.ops.hlist.Selector
import shapeless.ops.record.Keys
import shapeless.tag.Tagged
import shapeless.{HList, LabelledGeneric}

trait FieldName[A] {

  private type Field[T] = Symbol with Tagged[T]

  final class FieldNameGetter[S <: String] {
    def apply[Repr <: HList, KeysRepr <: HList]()(implicit
                                                  lbl: LabelledGeneric.Aux[A, Repr],
                                                  keys: Keys.Aux[Repr, KeysRepr],
                                                  selector: Selector[KeysRepr, Field[S]]): String =
      keys().select[Field[S]].name
  }

  def getFieldName[S <: String]: FieldNameGetter[S] = new FieldNameGetter[S]
}
