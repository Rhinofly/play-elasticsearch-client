lazy val root = project.in( file(".") )
  .settings(
                  name := "play-elasticsearch-client",
          organization := "net.kaliber",
          scalaVersion := "2.11.6",
    crossScalaVersions := Seq("2.10.6", scalaVersion.value)
  )

resolvers ++= Seq(
  "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"
)

val playVersion = "2.4.0"
val specs2Version = "2.4.15"

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play"         % playVersion % "provided",
  "com.typesafe.play" %% "play-ws"      % playVersion % "provided",
  "com.typesafe.play" %% "play-test"    % playVersion % "test",
  "com.typesafe.play" %% "play-specs2"  % playVersion % "test",
  "org.specs2"        %% "specs2-core"  % specs2Version % "test",
  "org.specs2"        %% "specs2-junit" % specs2Version % "test"
)

publishTo := {
  val repo = if (version.value endsWith "SNAPSHOT") "snapshot" else "release"
  Some("Kaliber Repository " + repo.capitalize + " Repository" at "https://jars.kaliber.io/artifactory/libs-" + repo + "-local")
}
