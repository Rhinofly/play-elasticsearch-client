import sbt._
import Keys._
import sbtrelease.ReleasePlugin.releaseSettings
import sbtrelease.ReleasePlugin.ReleaseKeys.crossBuild

object ApplicationBuild extends Build {

  val appName = "play-elasticsearch-client"

  val appDependencies = {
    val playVersion = "2.3.7"
    val specs2Version = "2.4.15"
      
    Seq(
      "com.typesafe.play" %% "play"         % playVersion,
      "com.typesafe.play" %% "play-ws"      % playVersion,
      "org.specs2"        %% "specs2-core"  % specs2Version % "test",
      "org.specs2"        %% "specs2-junit" % specs2Version % "test"
    )
  }

  val appScalaVersion = "2.11.4"

  val appCrossScalaVersions = Seq("2.10.3", "2.11.4")

  val appResolvers = Seq(
    "typesafe" at "http://repo.typesafe.com/typesafe/releases/"
  )

  def rhinoflyRepo(version: String) = {
    val repo = if (version endsWith "SNAPSHOT") "snapshot" else "release"
    Some("Rhinofly Internal " + repo.capitalize + " Repository" at "http://maven-repository.rhinofly.net:8081/artifactory/libs-" + repo + "-local")
  }

  val main = Project(appName, file(".")).enablePlugins(play.PlayScala).settings(releaseSettings:_*).settings(
    resolvers           :=  appResolvers,
    libraryDependencies ++= appDependencies,
    crossScalaVersions  :=  appCrossScalaVersions,
    scalaVersion        :=  appScalaVersion,
    organization        :=  "nl.rhinofly",
    publishTo           <<= version(rhinoflyRepo),
    credentials         +=  Credentials(Path.userHome / ".ivy2" / ".credentials"),
    scalacOptions       += "-feature",
    crossBuild          := true
  )
}
