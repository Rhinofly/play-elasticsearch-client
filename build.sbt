name := "play-elasticsearch-client"

version := "0.14-2.3-3-SNAPSHOT"

scalaVersion := "2.11.1"

organization := "nl.rhinofly"

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-ws" % "2.3.1",
  "org.specs2" %% "specs2" % "2.3.12" % "test"
)

publishTo := {
    val repo = if (version.value endsWith "SNAPSHOT") "snapshot" else "release"
    Some("Rhinofly Internal " + repo.capitalize + " Repository" at "http://maven-repository.rhinofly.net:8081/artifactory/libs-" + repo + "-local")
  }

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

scalacOptions += "-feature"

//ScoverageSbtPlugin.instrumentSettings

lazy val root = project in file(".")
