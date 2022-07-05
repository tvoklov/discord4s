name := "discord4s"
version := "0.1"

scalaVersion := "2.13.8"

val circeVersion = "0.14.2"
val scribeVersion = "3.10.0"
val http4sVersion = "1.0.0-M33"

libraryDependencies ++= Seq(
  "co.fs2" %% "fs2-core" % "3.2.8",

  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "io.circe" %% "circe-jawn" % circeVersion,

  "com.outr" %% "scribe" % scribeVersion,
  "com.outr" %% "scribe-slf4j" % scribeVersion,
  "com.outr" %% "scribe-cats" % scribeVersion,

  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-circe" % http4sVersion,
  "org.http4s" %% "http4s-jdk-http-client" % "1.0.0-M2"
)

// this helps properly closing IOApp
fork := true
