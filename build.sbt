name := "cats"

version := "0.1"

scalaVersion := "2.13.9"

val catsVersion = "2.8.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
)

scalacOptions ++= Seq(
  "-language:higherKinds"
)