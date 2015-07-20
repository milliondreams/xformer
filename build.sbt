name := "xformer"

version := "0.0.1"

scalaVersion := "2.10.5"

crossScalaVersions := Seq("2.10.5", "2.11.1")

resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play" % "2.3.0" % "provided",
  "com.typesafe.play" %% "play-json" % "2.3.0" % "provided"
)


