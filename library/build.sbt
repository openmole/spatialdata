//scalaVersion := "2.12.6"
scalaVersion := "2.11.8"

name := "spatialdata"

organization := "org.openmole"

version := "0.1-SNAPSHOT"

resolvers ++= Seq(
  "osgeo" at "http://download.osgeo.org/webdav/geotools",
  "geosolutions" at "http://maven.geo-solutions.it",
  "geotoolkit" at "http://maven.geotoolkit.org",
  "apache" at "http://repo.maven.apache.org/maven2",
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("staging"),
  Resolver.mavenLocal,
  Resolver.mavenCentral
)

val osmCommonVersion = "0.0.3-SNAPSHOT"
val geotoolsVersion = "18.4"

libraryDependencies ++= Seq(
  "org.apache.commons" % "commons-math3" % "3.6.1",
  "com.github.pathikrit" %% "better-files" % "3.5.0",
  "org.diana-hep" %% "histogrammar" % "1.0.4",
  "com.vividsolutions" % "jts" % "1.13",
  "org.scala-graph" %% "graph-core" % "1.12.5",
  "se.kodapan.osm.common" % "core" % osmCommonVersion,
  "se.kodapan.osm.common" % "java" % osmCommonVersion,
  "se.kodapan.osm.common" % "jts" % osmCommonVersion,
  "org.geotools" % "geotools" % geotoolsVersion exclude("javax.media", "jai_core"),
  "org.geotools" % "gt-shapefile" % geotoolsVersion exclude("javax.media", "jai_core"),
  "com.github.tototoshi" %% "scala-csv" % "1.3.4"
  //"javax.media" % "jai_core" % "1.1.3" //from "http://download.osgeo.org/webdav/geotools/javax/media/jai_core/1.1.3/jai_core-1.1.3.jar"
)



enablePlugins(SbtOsgi)

//lazy val omlplugin = Project("omlplugin", file("src")) settings(
  OsgiKeys.exportPackage := Seq("spatialdata.*;-split-package:=merge-first")
  OsgiKeys.importPackage := Seq("*;resolution:=optional")
  OsgiKeys.privatePackage := Seq("!scala.*,!java.*,!monocle.*,!META-INF.*.RSA,!META-INF.*.SF,!META-INF.*.DSA,META-INF.services.*,META-INF.*,*")
  OsgiKeys.requireCapability := """osgi.ee;filter:="(&(osgi.ee=JavaSE)(version=1.8))""""
//)

excludeFilter in unmanagedSources := HiddenFileFilter || "*kodapan*"

//libraryDependencies += "org.apache.httpcomponents" % "httpclient" % "4.3.5"
//libraryDependencies += "commons-io" % "commons-io" % "2.3"
//libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.1"
