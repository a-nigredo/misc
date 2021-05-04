name := "misc"

version := "0.1"

scalaVersion := "2.13.5"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3",
  "eu.timepit" %% "refined" % "0.9.24",
  "org.typelevel" %% "cats-core" % "2.4.2",
  "org.mongodb.scala" %% "mongo-scala-bson" % "4.2.3",
  "org.specs2" %% "specs2-core" % "4.10.0" % "test",
  "org.specs2" %% "specs2-cats" % "4.10.0" % "test",
)

scalacOptions ++= Seq(
  "-Ymacro-annotations",
  "-language:experimental.macros"
)

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.11.3" cross CrossVersion.full)