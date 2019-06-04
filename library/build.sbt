scalaVersion := "2.12.7"

name := "spatialdata"

organization := "org.openmole.library"

//version := "0.1-SNAPSHOT"
//version := "0.1"

resolvers ++= Seq(
  "osgeo" at "http://download.osgeo.org/webdav/geotools",
  "geosolutions" at "http://maven.geo-solutions.it",
  "geotoolkit" at "http://maven.geotoolkit.org",
  "apache" at "http://repo.maven.apache.org/maven2",
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("staging"),
  Resolver.mavenCentral//,
  //"Local Maven Repository" at "file:"+(new java.io.File(".")).getAbsolutePath+"/lib/maven",
  //Resolver.sbtIvyRepo("file:"+(new java.io.File(".")).getAbsolutePath+"/lib/ivy")
  //  Resolver.file("Local ivy", file( (new java.io.File(".")).getAbsolutePath+"/lib/ivy"))(Resolver.ivyStylePatterns)
)

val geotoolsVersion = "21.0"

libraryDependencies ++= Seq(
  "org.apache.commons" % "commons-math3" % "3.6.1",
  "com.github.pathikrit" %% "better-files" % "3.5.0",
  "com.vividsolutions" % "jts" % "1.13",
  //"org.openmole" %% "graph-core" % "1.12.6-SNAPSHOT",
  "org.openmole.library" %% "graph-core" % "1.12.5.1",
  "org.geotools" % "geotools" % geotoolsVersion exclude("javax.media", "jai_core") exclude("com.vividsolutions", "jts-core"),
  "org.geotools" % "gt-shapefile" % geotoolsVersion exclude("javax.media", "jai_core") exclude("com.vividsolutions", "jts-core"),
  "com.github.tototoshi" %% "scala-csv" % "1.3.4",
  "org.postgresql" % "postgresql" % "42.2.5",
  "org.mongodb" % "mongo-java-driver" % "3.10.0"
)



enablePlugins(SbtOsgi)

//lazy val omlplugin = Project("omlplugin", file("src")) settings(
  OsgiKeys.exportPackage := Seq("spatialdata.*;-split-package:=merge-first")
  OsgiKeys.importPackage := Seq("*;resolution:=optional")
  OsgiKeys.privatePackage := Seq("!scala.*,!java.*,!monocle.*,!META-INF.*.RSA,!META-INF.*.SF,!META-INF.*.DSA,META-INF.services.*,META-INF.*,*")
// FilteredSet,scala.collection.FilterableSet,scala.collection.EqSetFacade
//OsgiKeys.embeddedJars := Seq(new java.io.File("/Users/juste/.ivy2/cache/org.scala-graph/graph-core_2.12/jars/graph-core_2.12-1.12.5.jar"))
  OsgiKeys.requireCapability := """osgi.ee;filter:="(&(osgi.ee=JavaSE)(version=1.8))""""
//)

//excludeFilter in unmanagedSources := HiddenFileFilter || "*kodapan*"

libraryDependencies += "org.apache.httpcomponents" % "httpclient" % "4.3.5"
libraryDependencies += "commons-io" % "commons-io" % "2.3"
libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.1"


//mainClass in (Compile, packageBin) := Some("spatialdata.osm.OSMRealMeasures")
//mainClass in (Compile, packageBin) := Some("spatialdata.test.Test")

/*
assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.last
}
*/

// publish fat jar
// https://github.com/sbt/sbt-assembly#publishing-not-recommended
/*assemblyJarName in assembly := name+"_2.12.jar"
artifact in (Compile, assembly) := {
  val art = (artifact in (Compile, assembly)).value
  art.withClassifier(Some("assembly"))
}
addArtifact(artifact in (Compile, assembly), assembly)
*/


/**
  * Publishing
  */
//ThisBuild / organization := "org.openmole.library"

useGpg := true

publishMavenStyle in ThisBuild := true

publishTo in ThisBuild := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

// TODO remove when clean version released
publishConfiguration := publishConfiguration.value.withOverwrite(true)

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

licenses in ThisBuild := Seq("Affero GPLv3" -> url("http://www.gnu.org/licenses/"))

homepage in ThisBuild := Some(url("https://github.com/openmole/spatialdata"))

scmInfo in ThisBuild := Some(ScmInfo(url("https://github.com/openmole/spatialdata.git"), "scm:git:git@github.com:openmole/spatialdata.git"))

pomExtra in ThisBuild := (
  <developers>
    <developer>
      <id>justeraimbault</id>
      <name>Juste Raimbault</name>
    </developer>
    <developer>
      <id>julienperret</id>
      <name>Julien Perret</name>
    </developer>
  </developers>
  )

sonatypeProfileName := "org.openmole"

import sbtrelease.ReleasePlugin.autoImport.ReleaseTransformations._

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  setReleaseVersion,
  tagRelease,
  releaseStepCommand("publishSigned"),
  //setNextVersion,
  //commitNextVersion,
  releaseStepCommand("sonatypeRelease")
  //releaseStepCommand("sonatypeReleaseAll")//,
  //pushChanges
)

