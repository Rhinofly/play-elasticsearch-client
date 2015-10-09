Play 2.4 ElasticSearch Client
=============================

Description
-----------

This is an ElasticSearch client to be used in applications built in the Play framework.
It can be used with ElasticSearch version 1.0.1. Older and newer versions have a different API and will not work properly.

The Play 2.4 framework is required for its JSON handling capabilities.

Installation
------------

Add a resolver to your project settings:

``` scala
resolvers ++= Seq(
  "Kaliber Repository" at "https://jars.kaliber.io/artifactory/libs-release-local
)
```

Add the dependency:

``` scala
libraryDependencies ++= Seq(
  "net.kaliber" %% "play-elasticsearch-client" % "0.20"
  //Play 2.3
  "net.kaliber" %% "play-elasticsearch-client" % "0.19"
)
```


Simple usage 
------------

ElasticSearch offers many different API's, and this client reflects that. Therefore, it is impossible to give a short overview of the functionality.
See the unit tests for examples.


Release
-------------

> sbt release