name := "Mongala"

version := "0.1"

scalaVersion := "2.10.2"

resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
 "org.mongodb" %% "casbah" % "2.6.2",
 "play" %% "play" % "2.1.1"
)

