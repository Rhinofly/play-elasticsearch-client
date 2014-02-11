Play 2.1 ElasticSearch Client
=============================

Description
-----------

This is an ElasticSearch client to be used in applications built in the Play framework.
It can be used with ElasticSearch version 0.90.6 or higher.
The Play 2.1 (or higher) framework is required for its JSON handling capabilities.

Installation
------------

Add a resolver to your project settings:

``` scala
val main = play.Project(appName, appVersion, appDependencies, mainLang = SCALA).settings(
    resolvers += "Rhinofly Internal Repository" at "http://maven-repository.rhinofly.net:8081/artifactory/libs-release-local"
)
```

Add the dependency:

``` scala
	val appDependencies = Seq(
      "nl.rhinofly" %% "play-elasticsearch-client" % "x.y"
    )
```


Simple usage 
------------

``` scala

```

