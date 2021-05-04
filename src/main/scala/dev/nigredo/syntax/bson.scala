package dev.nigredo.syntax

import cats.data.NonEmptyList
import cats.syntax.unorderedFoldable._
import org.bson.BsonInvalidOperationException
import org.mongodb.scala.bson.{BsonArray, BsonDocument, BsonValue}

import scala.jdk.CollectionConverters.CollectionHasAsScala

object bson {

  implicit class Syntax(val value: BsonDocument) extends AnyVal {

    final type Result = Either[BsonInvalidOperationException, Boolean]

    final def valueNotExists(path: NonEmptyList[String], compare: BsonValue): Boolean =
      !valueExists(path, compare)

    final def valueExists(path: NonEmptyList[String], compare: BsonValue): Boolean = valueExists(path, _ == compare)

    final def valueExists(path: NonEmptyList[String], predicate: BsonValue => Boolean): Boolean = {
      def _valueExists(bson: BsonValue, path0: List[String]): Boolean = {
        path0 match {
          case Nil => predicate(bson)
          case l@h :: t =>
            bson match {
              case doc: BsonDocument => _valueExists(doc.get(h), t)
              case arr: BsonArray => arr.asScala.exists(_valueExists(_, l))
              case l => _valueExists(l, Nil)
            }
        }
      }

      _valueExists(value.get(path.head), path.tail)
    }

    final def keyNotExists(path: NonEmptyList[String]): Result =
      keyExists(path).map(!_)

    final def keyExists(path: NonEmptyList[String]): Result = {
      if (path.tail.isEmpty) Right(value.containsKey(path.head))
      else {
        value.get(path.head) match {
          case doc: BsonDocument => doc.keyExists(NonEmptyList.of(path.tail.head, path.tail.tail: _*))
          case arr: BsonArray =>
            Right(
              arr.asScala
                .exists(x => x.isDocument && x.asDocument().keyExists(NonEmptyList.of(path.tail.head, path.tail.tail: _*)).nonEmpty)
            )
          case x =>
            Left(
              new BsonInvalidOperationException(
                s"Unexpected document structure. Field '${path.head}' has type ${x.getBsonType.toString} but path is not exhausted. Left: ${path.tail.mkString(".")} Expected either Document or Array"
              )
            )
        }
      }
    }
  }
}
