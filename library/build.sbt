import sbt.Keys.publishMavenStyle
import sbt.enablePlugins

val geotoolsVersion = "27.0"

lazy val commonSettings = Seq(
  scalaVersion := "2.13.8",
  organization := "org.openmole.library",
  // trick to remove maven central (added again: change order for jai to be found in the continuous integration (sbt tries first resolver only in CI) since mavenCentral jai is broken (pom but no jar)
  externalResolvers := Resolver.combineDefaultResolvers(resolvers.value.toVector, mavenCentral = false),
  resolvers ++= Seq(
    "osgeo" at "https://repo.osgeo.org/repository/release/",
    "geosolutions" at "https://maven.geo-solutions.it/",
    "geotoolkit" at "https://maven.geotoolkit.org/",
    Resolver.mavenCentral
  ),
  libraryDependencies ++= Seq(
    "org.apache.commons" % "commons-math3" % "3.6.1",
    "org.apache.commons" % "commons-lang3" % "3.12.0",
    "org.apache.commons" % "commons-rng-sampling" % "1.4",
    "org.apache.commons" % "commons-rng-simple" % "1.4",
    "net.sourceforge.jdistlib" % "jdistlib" % "0.4.5",
    "com.github.pathikrit" %% "better-files" % "3.9.1",
    "org.geotools" % "gt-shapefile" % geotoolsVersion ,
    "org.geotools" % "gt-geopkg" % geotoolsVersion,
    "org.geotools" % "gt-referencing" % geotoolsVersion,
    "org.geotools" % "gt-epsg-hsql" % geotoolsVersion,
    "org.geotools" % "gt-epsg-extension" % geotoolsVersion,
    "org.geotools" % "gt-geotiff" % geotoolsVersion,
    "org.geotools" % "gt-coverage" % geotoolsVersion,
    "org.geotools" % "gt-opengis" % geotoolsVersion,
    "com.github.tototoshi" %% "scala-csv" % "1.3.10",
    "org.postgresql" % "postgresql" % "42.3.6",
    "org.mongodb" % "mongo-java-driver" % "3.12.11",
    "org.jgrapht" % "jgrapht-core" % "1.5.1",
    "org.apache.httpcomponents" % "httpclient" % "4.5.13",
    "commons-io" % "commons-io" % "2.11.0",
    "org.scalanlp" %% "breeze" % "2.0",
    "com.github.fommil.netlib" % "all" % "1.1.2",
    "de.ruedigermoeller" % "fst" % "3.0.3",
    "org.openstreetmap.pbf" % "osmpbf" % "1.5.0",
    "javax.media" % "jai-core" % "1.1.3" from "https://repo.osgeo.org/repository/release/javax/media/jai_core/1.1.3/jai_core-1.1.3.jar"
  ),
  cancelable in Global := true,
  ThisBuild / scalacOptions ++= Seq("-unchecked", "-deprecation","-feature")
)


lazy val overwriteNonSnapshot = true // use to overwrite when publish non-snapshot if issue during a previous release tentative
lazy val publishSettings = Seq(
  useGpg := true,
  ThisBuild / publishMavenStyle := true,
  ThisBuild / publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
    else Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  publishConfiguration := publishConfiguration.value.withOverwrite(overwriteNonSnapshot),
  //credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
  ThisBuild / licenses := Seq("Affero GPLv3" -> url("http://www.gnu.org/licenses/")),
  ThisBuild / homepage := Some(url("https://github.com/openmole/spatialdata")),
  ThisBuild / scmInfo := Some(ScmInfo(url("https://github.com/openmole/spatialdata.git"), "scm:git:git@github.com:openmole/spatialdata.git")),
  ThisBuild / pomExtra :=
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



lazy val spatialdata = project.in(file(".")).settings(
  Seq(name := "spatialdata") ++ commonSettings ++ publishSettings
)


/**
  * Testing with scalatest
  */
lazy val test = project.in(file("test")) dependsOn spatialdata settings(
  commonSettings ++ Seq(
    name := "test",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.12",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12"
  )
)

/**
  * OSGI bundle
  */
enablePlugins(SbtOsgi)

OsgiKeys.exportPackage := Seq("org.openmole.spatialdata.application.*;-split-package:=merge-first")
OsgiKeys.importPackage := Seq("*;resolution:=optional")
OsgiKeys.privatePackage := Seq("!java.*,!scala.*,*")
OsgiKeys.requireCapability := """osgi.ee; osgi.ee="JavaSE";version:List="1.8,1.9""""
// include geotools jars to have service files for factories
OsgiKeys.embeddedJars := (Compile / Keys.externalDependencyClasspath).value map (_.data) filter (f=> f.getName startsWith "gt-")


lazy val runtest = taskKey[Unit]("run test main class")
runtest := {
  Compile / mainClass := Some("org.openmole.spatialdata.test.Test")
  run / mainClass := Some("org.openmole.spatialdata.test.Test")
  run / fork := true
}


/**
  * Releasing: run releaseConfig, release
  */
lazy val releaseConfig = taskKey[Unit]("Release configuration")
releaseConfig := {
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
}

