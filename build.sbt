name := "xformer"

version := "0.0.1"

scalaVersion := "2.10.5"

crossScalaVersions := Seq("2.10.5", "2.11.1")

resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play" % "2.3.0" % "provided",
  "com.typesafe.play" %% "play-json" % "2.3.0" % "provided"
)

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := (
    <url>https://github.com/milliondreams/xformer</url>
    <licenses>
      <license>
        <name>Apache License 2.0</name>
        <url>http://www.apache.org/licenses/LICENSE-2.0</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:milliondreams/xformer.git</url>
      <connection>scm:git:git@github.com:milliondreams/xformer.git</connection>
    </scm>
    <developers>
      <developer>
        <id>milliondreams</id>
        <name>Rohit Rai</name>
        <url>http://twitter.com/milliondreams</url>
      </developer>
      <developer>
        <id>eraoferrors</id>
        <name>Shiti Saxena</name>
        <url>http://twitter.com/eraoferrors</url>
      </developer>
    </developers>)

