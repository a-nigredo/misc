package dev.nigredo

import scala.annotation.implicitNotFound
import scala.reflect.macros.blackbox

/**
 * Type class represents folding
 *
 * @tparam A accumulator
 * @tparam B value
 */
@implicitNotFound("No instance of Folder[${A}, ${B}] in scope")
trait Folder[A, B] {
  def fold(acc: A)(value: B): A
}

object Folder {

  sealed trait Boolean

  sealed trait True extends Boolean

  sealed trait False extends Boolean

  def instance[A, B](f: A => B => A): Folder[A, B] = new Folder[A, B] {
    override def fold(acc: A)(value: B): A = f(acc)(value)
  }

  def apply[A, B: Folder[A, *]]: Folder[A, B] = implicitly

  final class Builder[A, D <: Boolean, E <: Boolean] {
    def apply[B, C](empty: B, instance: C): B = macro foldMacro[B, C, A, D, E]
  }

  /**
   * Type-level folding
   *
   * Traverse type members val: T where T <: Marker and apply folding to them
   *
   * @tparam Marker        type members should extend it in order to be folded
   * @tparam FoldInherited define if inherited members should be folded as well
   * @return folding result
   */
  def fold[Marker, FoldInherited <: Boolean]: Builder[Marker, FoldInherited, False] =
    new Builder[Marker, FoldInherited, False]

  //Fold and print generated code
  def foldWithDebug[Marker, FoldInherited <: Boolean]: Builder[Marker, FoldInherited, True] =
    new Builder[Marker, FoldInherited, True]

  def foldMacro[A: c.WeakTypeTag, B: c.WeakTypeTag, C: c.WeakTypeTag, D <: Boolean : c.WeakTypeTag, E <: Boolean : c.WeakTypeTag](c: blackbox.Context)
                                                                                                                                 (empty: c.Expr[A], instance: c.Expr[B]): c.Expr[A] = {
    import c.universe._

    val aType = weakTypeOf[A]
    val bType = weakTypeOf[B]
    val cType = weakTypeOf[C]
    val debug = weakTypeOf[E].finalResultType.<:<(weakTypeOf[True])
    val useInherited = weakTypeOf[D].finalResultType.<:<(weakTypeOf[True])
    val bTypeInfo = bType.finalResultType.typeSymbol.info

    if (cType.finalResultType.<:<(weakTypeOf[Nothing]) || cType.finalResultType.<:<(weakTypeOf[Unit])) {
      c.error(c.enclosingPosition, s"Unsupported type '$cType'")
    }

    def isEqual(sym: Symbol, `type`: Type) =
      `type`.typeArgs.map(_.typeSymbol.typeSignature).find {
        case _: TypeBounds => true
        case _ => false
      } match {
        case None => sym.typeSignature.<:<(`type`)
        case _ => sym.typeSignature.baseClasses.contains(`type`.typeSymbol)
      }

    val values =
      (if (useInherited) bTypeInfo.members else bTypeInfo.decls).foldLeft(List.empty[Symbol]) {
        case (acc, x) =>
          x match {
            case v if v.isTerm && v.asTerm.isVal && isEqual(v, cType) => acc.+:(v)
            case _ => acc
          }
      }

    if (values.isEmpty)
      c.error(c.enclosingPosition, s"Not found subtype members of '$cType' in '$bType'")
    else
      c.info(c.enclosingPosition, s"Found subtype members ${values.map(_.toString).mkString("[", ",", "]")} of '$cType' in '$bType'", false)

    val code =
      q"""${
        values.map { x =>
          val name = TermName(x.name.toString.trim)
          val newName = TermName(c.freshName())
          q"""
             val $newName = $instance.$name
             Folder[$aType, ${x.typeSignature.finalResultType}].fold(_: ${empty.actualType})($newName)
           """
        }
      }.foldLeft[${empty.actualType}]($empty){
        case (acc, f) => f(acc)
      }"""

    if (debug) c.info(c.enclosingPosition, showCode(code), false)

    c.Expr[A](code)
  }
}
