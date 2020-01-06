scalaVersion := "2.13.1"

name := "spatialdata"

organization := "org.openmole.library"

resolvers ++= Seq(
  "osgeo" at "http://download.osgeo.org/webdav/geotools",
  "geosolutions" at "http://maven.geo-solutions.it",
  "geotoolkit" at "http://maven.geotoolkit.org",
  "apache" at "http://repo.maven.apache.org/maven2",
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("staging"),
  Resolver.mavenCentral
)

val geotoolsVersion = "21.0"

libraryDependencies ++= Seq(
  "org.apache.commons" % "commons-math3" % "3.6.1",
  "com.github.pathikrit" %% "better-files" % "3.8.0",
  "com.vividsolutions" % "jts" % "1.13",
  //"org.openmole.library" %% "graph-core" % "1.12.5.1", // graph-core not needed anymore
  "org.geotools" % "geotools" % geotoolsVersion exclude("javax.media", "jai_core") exclude("com.vividsolutions", "jts-core"),
  "org.geotools" % "gt-shapefile" % geotoolsVersion exclude("javax.media", "jai_core") exclude("com.vividsolutions", "jts-core"),
  "com.github.tototoshi" %% "scala-csv" % "1.3.6",
  "org.postgresql" % "postgresql" % "42.2.5",
  "org.mongodb" % "mongo-java-driver" % "3.10.0",
  "org.jgrapht" % "jgrapht-core" % "1.3.1",
  "org.apache.httpcomponents" % "httpclient" % "4.3.5",
  "commons-io" % "commons-io" % "2.3",
  "org.apache.commons" % "commons-lang3" % "3.1",
  "it.uniroma1.dis.wsngroup.gexf4j" % "gexf4j" % "1.0.0"
)



lazy val osmrealmeasures = Project("osmrealmeasures", file("target/osmrealmeasures")) settings(
  mainClass in (Compile, packageBin) := Some("org.openmole.spatialdata.application.urbmorph.OSMRealMeasures")
)

lazy val runtest = Project("runtest", file("target/test")) settings(
  mainClass in (Compile, packageBin) := Some("org.openmole.spatialdata.test.Test"),
  mainClass in run := Some("org.openmole.spatialdata.test.Test")
)


enablePlugins(SbtOsgi)

//lazy val omlplugin = Project("omlplugin", file("target/omlplugin")) enablePlugins SbtOsgi settings( // FIXME
//  name := "spatialdata",
 //org.openmole.spatialdata.application.*
  //OsgiKeys.exportPackage := Seq("*;-split-package:=merge-first")//,
  // export only application
  OsgiKeys.exportPackage := Seq("org.openmole.spatialdata.application")
  OsgiKeys.importPackage := Seq("*;resolution:=optional;-split-package:=merge-first")//,
// test private package to have only application visible
  OsgiKeys.privatePackage := Seq("org.openmole.spatialdata.grid,org.openmole.spatialdata.network,org.openmole.spatialdata.run,org.openmole.spatialdata.test,org.openmole.spatialdata.utils,org.openmole.spatialdata.vector,!scala.*,!java.*,!monocle.*,!META-INF.*.RSA,!META-INF.*.SF,!META-INF.*.DSA,META-INF.services.*,META-INF.*,*")//,
  OsgiKeys.requireCapability := """osgi.ee;filter:="(&(osgi.ee=JavaSE)(version=1.8))""""
//)


// publish fat jar
// https://github.com/sbt/sbt-assembly#publishing-not-recommended
lazy val assemble = Project("assemble", file("target/assemble")) settings (
  assemblyMergeStrategy in assembly := {
    case PathList("META-INF", xs @ _*) => MergeStrategy.discard
    case x => MergeStrategy.last
  },
  assemblyJarName in assembly := name+"_"+scalaVersion+".jar",
  artifact in (Compile, assembly) := {
    val art = (artifact in (Compile, assembly)).value
    art.withClassifier(Some("assembly"))
  },
  addArtifact(artifact in (Compile, assembly), assembly)
)


/**
  * Testing with scalatest
  */

libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" //% "test"



/**
  * Publishing
  */

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

/**
  * Releasing
  */


sonatypeProfileName := "org.openmole"

import sbt.enablePlugins
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

