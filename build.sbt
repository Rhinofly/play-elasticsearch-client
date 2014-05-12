name := "play-elasticsearch-client"

version := "0.14-2.3-2-SNAPSHOT"

organization := "nl.rhinofly"

libraryDependencies += ws

publishTo := {
    val repo = if (version.value endsWith "SNAPSHOT") "snapshot" else "release"
    Some("Rhinofly Internal " + repo.capitalize + " Repository" at "http://maven-repository.rhinofly.net:8081/artifactory/libs-" + repo + "-local")
  }

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

scalacOptions += "-feature"

ScoverageSbtPlugin.instrumentSettings

lazy val root = (project in file(".")).enablePlugins(PlayScala)

