Play 2.1 ElasticSearch Client
=====================================


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
      "nl.rhinofly" %% "play-elasticsearch-client" % "0.1"
    )
```


Simple usage 
------------

``` scala

```

