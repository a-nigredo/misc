package dev.nigredo.syntax

import cats.data.NonEmptyList
import org.specs2.mutable.Specification
import dev.nigredo.syntax.bson._
import org.mongodb.scala.bson.{BsonArray, BsonDocument, BsonString}
import org.specs2.specification.AllExpectations

class BsonSpec extends Specification with AllExpectations {

  "Key exists" in {
    BsonDocument("f1" -> "value").keyExists(NonEmptyList.of("f1")) must beRight(true)
    BsonDocument("f1" -> BsonDocument("f2" -> "value")).keyExists(NonEmptyList.of("f1", "f2")) must beRight(true)
    BsonDocument("f1" -> BsonArray(BsonDocument("f2" -> "value")))
      .keyExists(NonEmptyList.of("f1", "f2")) must beRight(true)
    BsonDocument("f1" -> BsonArray(BsonDocument("f2" -> "value"), "value"))
      .keyExists(NonEmptyList.of("f1", "f2", "f3", "f4")) must beRight(false)
    BsonDocument("f1" -> "23")
      .keyExists(NonEmptyList.of("f1", "f2", "f3")) must beLeft
  }

  "Key not exists" in {
    BsonDocument("f1" -> BsonArray(BsonDocument("f2" -> "value"), "value"))
      .keyNotExists(NonEmptyList.of("f1", "f2", "f3", "f4")) must beRight(true)
    BsonDocument("f1" -> BsonDocument("f2" -> "value")).keyNotExists(NonEmptyList.of("f1", "f25")) must beRight(true)
  }

  "Value exist" in {
    BsonDocument("f1" -> "value").valueExists(NonEmptyList.of("f1"), BsonString("value")) must beTrue
    BsonDocument("f1" -> BsonDocument("f2" -> "value")).valueExists(NonEmptyList.of("f1", "f2"), BsonString("value")) must beTrue
    BsonDocument("f1" -> BsonArray(BsonDocument("f2" -> BsonDocument("f3" -> "value")))).valueExists(NonEmptyList.of("f1", "f2", "f3"), BsonString("value")) must beTrue
  }

  "Value not exist" in {
    BsonDocument("f1" -> "value").valueExists(NonEmptyList.of("f1"), BsonString("value2")) must beFalse
    BsonDocument("f1" -> BsonDocument("f2" -> "value")).valueExists(NonEmptyList.of("f1", "f2"), BsonString("value2")) must beFalse
    BsonDocument("f1" -> BsonArray(BsonDocument("f2" -> BsonDocument("f3" -> "value")))).valueExists(NonEmptyList.of("f1", "f2", "f3"), BsonString("value2")) must beFalse
  }
}

